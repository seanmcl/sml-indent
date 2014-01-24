
structure Set =
   OrdSetExtFn
      (struct
          type ord_key = t
          val compare = compare
          val ppItem = pp
       end)
