
val _ =
   let in
      if Clock.time depthClock mod depthInterval = 0 then
         case Index.find(index, id) of
            SOME x => 5
      else
         5
   end
