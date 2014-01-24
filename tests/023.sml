
fun f x  =
   let
      val emptybucket = Weak.create 0
   in
      Array.modify (fn _ => emptybucket) t
    ; totsize := 0
    ; limit := 3
   end

