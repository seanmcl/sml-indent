(*
  Indenting a line consists of three phases
  1) Close some number of intermediate open blocks based on the first symbol.
  2) Indent line according to the first symbol
  3) Walk through all the symbols, closing any corresponding blocks
*)
structure S = Sym

structure Config = struct
   val basic = 3
end

structure Lex = struct
   structure Comment : sig
      val push : unit -> unit
      val pop : unit -> unit
      val in_ : unit -> bool
      val level : unit -> int
   end = struct
      val comment = ref 0
      fun push () = Ref.incr comment
      fun pop () = Ref.decr comment
      fun in_ () = !comment > 0
      fun level () = !comment
   end

   structure String : sig
      val toggle : unit -> unit
      val in_ : unit -> bool
   end = struct
      val comment = ref false
      fun toggle () = comment := not (!comment)
      fun in_ () = !comment
   end
end

structure Parse :> sig
   val parse : string -> Sym.t list
end = struct
   structure Lexer = IndentLexer
   fun parse s =
      let
         val s = if Lex.Comment.in_ () then ";COMMENT;" ^ s
                 else if Lex.String.in_ () then ";STRING;" ^ s else s
         fun stringReader s =
            let val next = ref s in
               fn _ => !next before next := ""
            end
         val lexer : unit -> Sym.t = Lexer.makeLexer (stringReader s)
         fun loop acc =
            let val t = lexer () in
               (* Debug.print ["Read: ", Sym.toString t, "\n"]; *)
               case t of
                  S.Eof => rev acc
                | _ => loop (t :: acc)
            end
      in
         loop []
      end
end

structure Line : sig
   type t
   val make : int * string -> t
   val indent : t -> int
   val origIndent : t -> int
   val setIndent : t * int -> unit
   val isComment : t -> bool
   val assertComment : t * int -> unit
   val commentIndent : t -> int
   val toString : t -> string
   val syms : t -> Sym.t list
   val num : t -> int
   val isEmpty : t -> bool
   val last : t -> Sym.t option
   val debugString : t -> string
end = struct
   datatype t = L of
      { indent : int ref
      , origIndent : int
      , lineNum : int
      , contents : string
      , syms : Sym.t list
      , commentIndent : int option ref }

   fun debugString (L { indent, origIndent, lineNum, contents,
                        commentIndent, ... }) =
      let
         val c = case !commentIndent of
            NONE => ""
          | SOME n => Int.toString n
      in
         String.concat [ "(i=", Int.toString (!indent), ", o=", Int.toString origIndent
                       , ", n=", Int.toString lineNum, ", c=", c, ") : ", contents ]
      end
   val _ = debugString

   fun make (lineNum, s) =
      let
         val syms = Parse.parse s
         fun currentIndent s =
            let
               val n = String.size s
               fun loop k =
                  if k = n then
                     (* a line of only spaces *)
                     0
                  else
                     if String.sub(s, k) = #" " then loop (k+1)
                     else k
            in
               if n = 0 then 0 else loop 0
            end
         val origIndent = currentIndent s
         val indent = ref origIndent
         val contents = String.losespecl String.whitespec s
      in
         Debug.print [ Int.toString (lineNum+1), " : "];
         List.app (fn s => Debug.print [ S.toString s, "," ]) syms;
         Debug.print [ "\n" ];
         L { indent = indent
           , origIndent = origIndent
           , lineNum = lineNum + 1
           , contents = contents
           , syms = syms
           , commentIndent = ref NONE }
      end
   fun syms (L { syms, ... }) = syms
   fun isComment (L { commentIndent, ... }) = isSome (!commentIndent)
   fun indent (L { indent, ... }) = !indent
   fun origIndent (L { origIndent, ... }) = origIndent
   fun num (L { lineNum, ... }) = lineNum
   fun setIndent (L { indent, ... }, n) = indent := n
   fun assertComment (L { commentIndent, ... }, n) = commentIndent := SOME n
   fun commentIndent (L { commentIndent, ...}) = valOf (!commentIndent)
   fun toString (L { indent = ref n, contents = s, ... }) = String.space n ^ s
   fun isEmpty (L { contents, ... }) = contents = ""
   fun last (L { syms, ...}) = case syms of
      [] => NONE
    | _ => SOME (List.last syms)
end

(* A Mode.t is a block opening *)
structure Mode = struct
   datatype t =
      Top
    (* arg is the location of the last signature/structure/functor *)
    | SigStruct of int option
    | Dec of int
    | Expr of int
    (* datatype is not quite like a normal dec, as you need to
       know where the = sign is. *)
    | Datatype of int option
    | Withtype
    | LetLoc
    | CaseFn of int option
    | If of int
    | Else of int
    | Include of int
    | Wheretype of int
    (* bracket pairs *)
    | Lbrace of int
    | Lbrack of int
    | Lparen of int
    (* Messy stuff *)
    | Quote of int
    | Bug of string

   (* close the remaining blocks. *)
   val matches = fn
      (SigStruct _, S.End) => true
    | (LetLoc, S.End) => true
    | (If _, S.Else) => true
    | (Lbrace _, S.Rbrace) => true
    | (Lbrack _, S.Rbrack) => true
    | (Lparen _, S.Rparen) => true
    | (Quote _, S.DoubleQuote _) => true
    | _ => false

   (* Symbol close blocks. *)
   fun closes (m, s) =
      not (matches (m, s)) andalso
      let
         fun dec () = List.mem [ S.In, S.End, S.Rparen, S.Rbrack, S.And ] s
            orelse S.startsDec s
            orelse
            case s of
               S.Local _ => true
             | _ => false
      in
         case m of
            Dec _ => dec ()
          | Datatype _ => dec () orelse s = S.Withtype
          | Withtype => S.startsDec s
          | Include _ => S.startsDec s orelse List.mem [S.End] s
          | Wheretype _ => S.startsDec s orelse
            List.mem [ S.Rparen, S.Rbrace, S.Rbrack ] s
            orelse
            let in
               case s of
                  S.Include _ => true
                | S.End => true
                | S.Struct _ => true
                | _ => false
            end
          | CaseFn _ =>
            List.mem [ S.Else, S.Then, S.In, S.End, S.Rparen, S.Semi
                     , S.And, S.Rbrace, S.Rbrack, S.Comma ] s
            orelse S.startsDec s
            orelse
            let in
               case s of
                  S.Local _ => true
                | _ => false
            end
          | If _ =>
            List.mem [ S.Else ] s
          | Else _ =>
            List.mem [ S.Else, S.In, S.End, S.Bar, S.Rparen, S.Rbrack, S.Semi
                     , S.And ] s
            orelse S.startsDec s
          | Lbrace _ =>
            List.mem [ S.End, S.In, S.Rparen, S.Rbrack, S.And ] s
            orelse S.startsDec s
          | Lbrack _ =>
            List.mem [ S.End, S.In, S.Rparen, S.Rbrace, S.And ] s
            orelse S.startsDec s
          | Lparen _ =>
            List.mem [ S.End, S.In, S.Rbrack, S.Rbrace, S.And ] s
            (* orelse S.startsDec s *)
          | Expr _ => S.endsExpr s
          | _ => false
      end

   fun int s n = String.concat [s, "(", Int.toString n, ")" ]

   fun toString m = case m of
      Top => "Top"
    | SigStruct NONE => "SigStruct(NONE)"
    | SigStruct (SOME n) => int "SigStruct" n
    | Dec n => int "Dec" n
    | Expr n => int "Expr" n
    | Datatype NONE => "Datatype(NONE)"
    | Datatype (SOME n) => int "Datatype" n
    | Withtype => "Withtype"
    | Wheretype n => int "Wheretype" n
    | LetLoc => "LetLoc"
    | CaseFn NONE => "CaseFn(NONE)"
    | CaseFn (SOME n) => int "CaseFn" n
    | If n => int "If" n
    | Else n => int "Else" n
    | Include n => int "Include" n
    | Lbrace n => int "Lbrace" n
    | Lbrack n => int "Lbrack" n
    | Lparen n => int "Lparen" n
    | Quote n => int "Quote" n
    | Bug s => "Bug: " ^ s
end

structure Comment :> sig
   (* push the line and it's newly calculated indentation. *)
   val push : Line.t * int -> unit
   val pop : Line.t -> unit
   val step : unit -> int option
end = struct
   structure C = Lex.Comment
   val step = ref 0

   fun push (line, n) =
      let in
         (if C.in_ () then () else step := n);
         C.push ();
         Debug.printl
            [ Int.toString (Line.num line), ": Pushed comment (level = "
            , Int.toString (C.level ()), ", step = "
            , Int.toString (!step), ")" ]
      end

   fun pop line =
      let in
         (if C.in_ () then () else step := 0);
         C.pop ();
         Debug.printl
            [ Int.toString (Line.num line), ": Popped comment (level = "
            , Int.toString (C.level ()), ", step = ", Int.toString (!step)
            , ")" ]
      end

   val step = fn () => if Lex.Comment.in_ () then SOME (!step) else NONE
end

structure Stack :> sig
   val push : Line.t -> Mode.t * int -> unit
   val pop : Line.t -> unit
   val peek : unit -> Mode.t * int
   val peekMode : unit -> Mode.t
   val lastClosed : unit -> Mode.t * int
end = struct
   structure D = Debug
   structure M = Mode

   val stack : (M.t * int) list ref = ref [ (M.Top, 0) ]

   val last = ref (M.Top, 0)

   fun lastClosed () = !last

   fun peek () =
      case !stack of
         m :: _ => m
       | [] => raise (Fail "Impossible")

   fun peekMode () =
      case !stack of
         (m, _) :: _ => m
       | [] => raise (Fail "Impossible")

   fun push line (m, n) =
      (* Don't push 2 exprs in a row. *)
      case (m, peek ()) of
         (M.Expr _, (M.Expr _, _)) =>
         let in
            Debug.print ["Skipping expr\n"];
            ()
         end
       | _ =>
         let val k = length (!stack) in
            D.printl [ String.space (2 * k), "> ", Int.toString (Line.num line), " "
                     , M.toString m ];
            stack := (m, n) :: (!stack)
         end

   fun pop line =
      case !stack of
         (m, n) :: s =>
         let val k = length s in
            D.printl [ String.space (2 * k), "< ", Int.toString (Line.num line), " "
                     , M.toString m ]
          ; stack := s
          ; last := (m, n)
         end
       | [] => raise (Fail "Impossible")
end

structure Indent = struct
   structure M = Mode
   structure C = Config
   structure D = Debug

   fun makeLineArray currentLineNum (lines : string array) =
      let
         val previousLine = ref NONE
         fun mapFn (lineNum, line) =
            let
               val line = Line.make (lineNum, line)
               val (firstSym, syms) = case Line.syms line of
                  (* Make up a dummy symbol for lines with no indent chars *)
                  [] => (S.Eof, [])
                | sym :: syms => (sym, syms)
               val (lastSym, syms) = case rev syms of
                  [] => (NONE, [])
                | sym :: syms => (SOME sym, rev syms)
               (* Identify comments *)
               val _ = case (firstSym, Comment.step ()) of
                  (S.Cstart n, _) => Line.assertComment(line, n)
                | (_, SOME n) => Line.assertComment(line, n)
                | _ => ()
               (* 1. Close modes. *)
               fun pop1 () = Stack.pop line
               fun close sym =
                  let
                     val m = Stack.peekMode ()
                  in
                     if M.closes (m, sym) then (pop1 (); close sym) else ()
                  end
               fun match sym =
                  let
                     val m = Stack.peekMode ()
                  in
                     if M.matches (m, sym) then pop1 () else ()
                  end
               val () = if S.closeBeforeIndent firstSym
                        then close firstSym else ()
               (* 2. Indent. *)
               val thisIndent =
                  if Line.num line <> currentLineNum andalso Line.isEmpty line
                  then 0
                  else
                     let
                        fun error msg =
                           ( D.printl [ "ERROR: line ",
                                       Int.toString (Line.num line), ": ", msg ]
                           ; Line.indent line )
                     in
                        case (firstSym, Stack.peek ()) of
                           (_             , (M.Top, n)      ) => n
                         | (S.In          , (M.LetLoc, n)   ) => n
                         | (S.End         , (M.SigStruct (SOME k), _)) => k
                         | (S.End         , (M.SigStruct _, n)) => n
                         | (S.End         , (M.LetLoc, n)   ) => n
                         | (S.End         , _               ) => error "end1"
                         | (S.Fun _       , (M.SigStruct (SOME k), _)) => k + C.basic
                         | (S.Val _       , (M.SigStruct (SOME k), _)) => k + C.basic
                         | (S.Structure _ , (M.SigStruct (SOME k), _)) => k + C.basic
                         | (S.Functor  _  , (M.SigStruct (SOME k), _)) => k + C.basic
                         | (S.Local _     , (M.SigStruct (SOME k), _)) => k + C.basic
                         | (S.Open _      , (M.SigStruct (SOME k), _)) => k + C.basic
                         | (S.Datatype _  , (M.SigStruct (SOME k), _)) => k + C.basic
                         | (S.Type _      , (M.SigStruct (SOME k), _)) => k + C.basic
                         | (S.Exception _ , (M.SigStruct (SOME k), _)) => k + C.basic
                         | (S.Infix _     , (M.SigStruct (SOME k), _)) => k + C.basic
                         | (S.Fun _       , (M.SigStruct _, n)) => n + C.basic
                         | (S.Fn _        , (M.CaseFn (SOME k), _)) => k
                         | (S.FnArr _     , (M.CaseFn (SOME k), _)) => k
                         | (S.Bar         , (M.CaseFn NONE, n)     ) => n + 1
                         | (S.Bar         , (M.CaseFn (SOME k), _)) => k + 1
                         | (S.Bar         , (M.Dec _, n)      ) => n + 2
                         | (S.Bar         , (M.Datatype NONE, n)) => n + 1
                         | (S.Bar         , (M.Datatype (SOME k), _)) => k
                         | (S.Comma       , (M.Lbrace k, _)) => k
                         | (S.Comma       , (M.Lbrack k, _)) => k
                         | (S.Comma       , (M.Lparen k, _)) => k
                         | (S.Rbrace      , (M.Lbrace k, _)) => k
                         | (S.Rbrack      , (M.Lbrack k, _)) => k
                         | (S.Rparen      , (M.Lparen k, _)) => k
                         | (S.Then        , (M.If k, _)) => k
                         | (S.Else        , (M.If k, _)) => k
                         | (S.ElseIf      , (M.If k, _)) => k
                         | (S.Semi        , (M.Lparen k, _)) => k
                         | (S.Semi        , (M.LetLoc, n)) => n + 1
                         | (S.And         , (M.Withtype, n)) => n + 5
                         | (S.Of          , (M.CaseFn (SOME k), _)) => k
                         | (S.Andalso     , (M.Expr k, _)) => k
                         | (S.Orelse      , (M.Expr k, _)) => k
                         | (S.Andtype     , (M.Wheretype k, _)) => k + 2
                         | (S.Wheretype _ , (M.Wheretype k, _)) => k
                         | (S.Andtype     , (_, n)) => n + C.basic + 2
                         | (_             , (M.Dec k, _)) => k + C.basic
                         | (_             , (M.Quote k, _)  ) => k
                         | (_             , (M.Lbrace k, _) ) => k + 2
                         | (_             , (M.CaseFn (SOME k), n)) =>
                           let in
                              case !previousLine of
                                 NONE => k + C.basic
                               | SOME l =>
                                 case Line.last l of
                                    SOME (S.FnArr _) => n + C.basic
                                  | _ => k + C.basic
                           end
                         (* | (_             , (M.CaseFn (SOME k), _)) => k + C.basic *)
                         | (_             , (M.Lbrack k, _) ) => k + 1
                         | (_             , (M.Lparen k, _) ) => k + 1
                         | (_             , (M.If k, _) ) => k + C.basic
                         | (_             , (M.Expr k, _)) => k + C.basic
                         | (_             , (_, n)) => n + C.basic
                     end
               val () = if S.closeAfterIndent firstSym
                        then close firstSym else ()
               (* 3. Update stack. *)
               fun push isLast sym =
                  let
                     fun push m =
                        if Lex.Comment.in_ () then ()
                        else Stack.push line (m, thisIndent)
                     fun adjust n = Int.max (0, n - Line.origIndent line + thisIndent)
                     fun caseFn n =
                        if S.startsDec firstSym
                           orelse
                           (List.mem [ S.And, S.Arr, S.Colon ] firstSym
                            andalso case lastSym of
                                       SOME (S.Fn _) => false
                                     | _ => true)
                           orelse
                           case (firstSym, lastSym) of
                              (S.Fn _, SOME (S.Fn _)) => true
                            | (S.FnArr _, SOME (S.Fn _)) => true
                            | (S.Fn _, SOME lastSym) =>
                              List.mem [ S.Of ] lastSym
                            | (S.FnArr _, SOME lastSym) =>
                              List.mem [ S.Of ] lastSym
                            | _ => false
                        then push (M.CaseFn NONE)
                        else if isLast then
                           case Stack.peek () of
                              (M.Dec _, n) => push (M.CaseFn (SOME n))
                            | _ => push (M.CaseFn (SOME (adjust n)))
                        else push (M.CaseFn (SOME (adjust n)))
                     fun pushDatatype () = case Line.last line of
                        NONE => raise (Fail "Impossible")
                      | SOME (S.Eq _) => push (M.Datatype NONE)
                      | SOME _ =>
                        case List.filter (fn (S.Eq _) => true | _ => false) syms of
                           [] => push (M.Bug "No = on datatype line")
                         | S.Eq n :: _ => push (M.Datatype (SOME (adjust n)))
                         | _ => raise (Fail "Impossible")
                  in
                     case (sym, Stack.peek ()) of
                        (S.Struct _, (M.Dec n, _)) => push (M.SigStruct (SOME n)) (* n from the Dec is already adjusted *)
                      | (S.Struct k, _) => push (M.SigStruct (SOME (adjust k)))
                      | (S.Sig, _) => push (M.SigStruct NONE)
                      | (S.Let, _) => push M.LetLoc
                      | (S.Local _, _) => push M.LetLoc
                      | (S.Val n, _) => push (M.Dec (adjust n))
                      | (S.Fun n, _) => push (M.Dec (adjust n))
                      | (S.Structure n, _) => push (M.Dec (adjust n))
                      | (S.Signature n, _) => push (M.Dec (adjust n))
                      | (S.Functor n, _) => push (M.Dec (adjust n))
                      | (S.Infix n, _) => push (M.Dec (adjust n))
                      | (S.Exception n, _) => push (M.Dec (adjust n))
                      | (S.Type n, _) => push (M.Dec (adjust n))
                      | (S.Case n, _) => caseFn n
                      | (S.Fn n, _) => caseFn n
                      | (S.FnArr n, _) =>
                        if isLast then push (M.CaseFn NONE) else caseFn n
                      | (S.Handle n, _) => caseFn n
                      | (S.If n, _) => push (M.If (adjust n))
                      | (S.Datatype _, _) => pushDatatype ()
                      | (S.Withtype, _) => push M.Withtype
                      | (S.Include n, _) => push (M.Include (adjust n))
                      | (S.Wheretype _, (M.Wheretype _, _)) => ()
                      | (S.Wheretype n, _) => push (M.Wheretype (adjust n))
                      | (S.And, _) =>
                        let in
                           case Stack.lastClosed () of
                              (M.Datatype _, _) => pushDatatype ()
                            | (M.Dec n, _) => push (M.Dec (adjust n))
                            | _ => ()
                        end
                      | (S.Else, _) =>
                        if isLast then () else
                        let in
                           case Stack.lastClosed () of
                              (M.If k, _) => push (M.Else k)
                            | _ => push (M.Else 0)
                        end
                      | (S.Lbrace n, _) => push (M.Lbrace (adjust n))
                      | (S.Lbrack n, _) => push (M.Lbrack (adjust n))
                      | (S.Lparen n, _) => push (M.Lparen (adjust n))
                      (* Don't adjust.  Respect original indentation. *)
                      | (S.Cstart n, _) => Comment.push (line, n)
                      | (S.Cend, _) => Comment.pop line
                      | (S.DoubleQuote n, _) =>
                        let in
                           if Lex.String.in_ () then () else push (M.Quote (adjust n))
                         ; Lex.String.toggle ()
                        end
                      | (S.Id n, _) => push (M.Expr (adjust n))
                      | _ => ()
                  end
               fun f isLast sym = (close sym; match sym; push isLast sym)
            in
               match firstSym
             ; push false firstSym
             ; (List.app (f false) syms)
             ; case lastSym of SOME sym => f true sym | NONE => ()
             ; Line.setIndent (line, thisIndent)
             ; previousLine := SOME line
             ; line
            end
      in
         Array.mapi mapFn lines
      end

   fun fixComments lines =
      let
         val n = Array.length lines
         val k = ref (n-1)
         val ind = ref 0
      in
         while !k >= 0 do
            let
               val line = Array.sub(lines, !k)
            in
               if Line.isComment line
               then Line.setIndent
                       ( line,
                        Int.max(0, !ind + Line.origIndent line - Line.commentIndent line) )
               else
                  if Line.isEmpty line then () else ind := Line.indent line
             ; k := !k - 1
            end
      end

   fun doit currentLineNum =
      let
         val lines = TextIO.lines ()
         val lines = makeLineArray currentLineNum lines
      in
         fixComments lines;
         Array.appi (fn (i, l) =>
                        let in
                           ignore i;
                           (* D.print [ Int.toString (i+1), " : " ]; *)
                           (* D.print [ Line.debugString l, "\n" ]; *)
                           print (Line.toString l);
                           (* D.print (if Line.isComment l then [ " (\* c", Int.toString (Line.commentIndent l), " *\)" ] else []); *)
                           print "\n"
                        end) lines
      end
end
