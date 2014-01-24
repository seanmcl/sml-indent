
structure Sym = struct
   datatype t =
      Arr
    | And
    | Andalso
    | Andtype
    | Backslash
    | Bar
    | Case of int
    | Cend
    | Char
    | Colon
    | Comma
    | Cstart of int
    | Darr
    | Datatype of int
    | DoubleQuote of int
    | Else
    | ElseIf
    | End
    | Eof
    | Eq of int
    | Exception of int
    | Fn of int
    | FnArr of int
    | Fun of int
    | Functor of int
    | Handle of int
    | Id of int
    | If of int
    | In
    | Include of int
    | Infix of int
    | Lbrace of int
    | Lbrack of int
    | Let
    | Local of int
    | Lparen of int
    | Of
    | Opaque
    | Open of int
    | Orelse
    | Rbrace
    | Rbrack
    | Rparen
    | Semi
    | Sig
    | Signature of int
    | SingleQuote
    | Star
    | Struct of int
    | Structure of int
    | Then
    | Type of int
    | Val of int
    | Wheretype of int
    | Withtype

   val startsDec = fn
      Structure _ => true
    | Signature _ => true
    | Functor _ => true
    | Include _ => true
    | Val _ => true
    | Fun _ => true
    | Datatype _ => true
    | Type _ => true
    | Exception _ => true
    | Open _ => true
    | Infix _ => true
    | Local _ => true
    | _ => false

   fun endsExpr s =
      startsDec s
      orelse
      List.mem [Bar, Then, Else, ElseIf, Rbrace, Rparen, Rbrack, Comma
               , In, End, Semi, And, Arr, Of, Bar, Darr, Andalso, Orelse
               , Withtype, Andtype, Colon, Opaque, Star, Sig ] s
      orelse
      case s of
         Eq _ => true
       | Handle _ => true
       | Fn _ => true
       | FnArr _ => true
       | Wheretype _ => true
       | Include _ => true
       | Struct _ => true
       | _ => false

   fun int s n = String.concat [s, "(", Int.toString n, ")" ]

   (* val closeAfterIndent = fn *)
   (*    Orelse => true *)
   (*  | Andalso => true *)
   (*  | _ => false *)

   fun closeAfterIndent _ = false

   fun closeBeforeIndent s = not (closeAfterIndent s)

   fun toString t = case t of
      And => "And"
    | Andalso => "Andalso"
    | Arr => "Arr"
    | Backslash => "Backslash"
    | Bar => "Bar"
    | Case n => int "Case" n
    | Cend => "Cend"
    | Char => "Char"
    | Colon => "Colon"
    | Comma => "Comma"
    | Cstart n => int "Cstart" n
    | Darr => "Darr"
    | Datatype n => int "Datatype" n
    | DoubleQuote n => int "DoubleQuote" n
    | Else => "Else"
    | ElseIf => "ElseIf"
    | End => "End"
    | Eof => "Eof"
    | Eq n => int "Eq" n
    | Exception n => int "Exception" n
    | Fn n => int "Fn" n
    | FnArr n => int "FnArr" n
    | Fun n => int "Fun" n
    | Functor n => int "Functor" n
    | Handle n => int "Handle" n
    | Id n => int "Id" n
    | If n => int "If" n
    | In => "In"
    | Include n => int "Include" n
    | Infix n => int "Infix" n
    | Lbrace n => int "Lbrace" n
    | Lbrack n => int "Lbrack" n
    | Let => "Let"
    | Local n => int "Local" n
    | Lparen n => int "Lparen" n
    | Of => "Of"
    | Opaque => "Opaque"
    | Open n => int "Open" n
    | Orelse => "Orelse"
    | Rbrace => "Rbrace"
    | Rbrack => "Rbrack"
    | Rparen => "Rparen"
    | Semi => "Semi"
    | Sig => "Sig"
    | Signature n => int "Signature" n
    | SingleQuote => "SingleQuote"
    | Star => "Star"
    | Struct n => int "Struct" n
    | Structure n => int "Structure" n
    | Then => "Then"
    | Type n => int "Type" n
    | Val n => int "Val" n
    | Wheretype n => int "Wheretype" n
    | Andtype => "Andtype"
    | Withtype => "Withtype"
end
