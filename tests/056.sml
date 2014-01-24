
structure Prop :> Prop = struct

   structure P = SymFn(val defaultName = "p")
   open P

   fun sign p = if P.isFixed p then Util.Pos else Util.Neg

end
