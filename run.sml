
structure Args = struct
   type t = { debug : bool, line : int, help : bool }

   fun parse () : t =
      let
         val debug = ref false
         val line = ref NONE
         val help = ref false
         val rec loop = fn
            "-debug" :: rest => (debug := true; loop rest)
          | "-help" :: rest => (help := true; loop rest)
          | "-line" :: n :: rest => (line := Int.fromString n; loop rest)
          | a :: _ => raise (Fail ("Unknown flag: " ^ a))
          | [] =>
            let
               val line = case !line of
                  NONE => ~1
                | SOME n => n
            in
               { debug = !debug, line = line, help = !help }
            end
      in
         loop (CommandLine.arguments ())
      end
end

val _ =
   let
      val { debug, line, help } = Args.parse ()
   in
      if help
      then print "Usage: indent < file [-help|-line N|-debug]\n"
      else let in
         if debug then Debug.debug := true else ()
       ; Indent.doit line
      end
   end
