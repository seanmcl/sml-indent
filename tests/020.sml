
fun makeLineArray lines reset =
   let
      val n = Array.length lines
      val lines = Array.mapi Line.make lines
      fun appFn (k, line) =
         let
            val _ = if Stack.inComment () then Line.assertComment line else ()
            val origIndent = Line.origIndent line
         in
            5
         end
   in
      6
   end


