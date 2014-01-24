
let
   val _ =
      fn _ => 5

   local
      open PP.Ops
   in
      val ppShort = ppStats o stats
   end
in
   5
end
