
val _ =
   if _
   then
      Debug.pp (fn () => %[$(Util.pad "Rejecting subsumed rule"), Rule.pp rule])
   else
      if
         y
      then
         Debug.pp (fn () => %[$(Util.pad "Rejecting equal rule"), Rule.pp rule])
      else let in
         z
      end
