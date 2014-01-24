
val _ =
   let in
      5
    ; if Clock.time depthClock mod depthInterval = 0
      then
         case TQ.min timeQueue of
            NONE => raise Impossible
          | SOME (_, id) => 7
      else 8
   end
