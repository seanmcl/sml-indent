
val _ =
   case _ of
      NONE => ()
    | SOME x =>
      if _
      then ()
      else raise Fail "Error: This node has children"
