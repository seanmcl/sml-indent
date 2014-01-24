fun eq (S (_, ants1, cons1), S (_, ants2, cons2)) =
   Cons.eq (cons1, cons2) andalso Ants.eq (ants1, ants2)

local open Cons in
   fun subsumesC (P l, P l') = Prop.eq(l, l')
     | subsumesC (P _, Xi) = false
     | subsumesC (Xi, _) = true
end

fun subsumes (S (_, ants, cons), S (_, ants', cons')) =
   (* A simple filter *)
   Ants.numItems ants <= Ants.numItems ants' andalso
   subsumesC (cons, cons') andalso
   Ants.isSubset (ants, ants')
