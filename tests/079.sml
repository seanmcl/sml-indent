
val _ =
   if p then NONE else
   case consSubst (cons1, cons2) of
      x => x
