
structure List = struct
   fun mem l x = List.exists (fn y => x = y) l
   open List
end

structure Array = struct
   structure A = Array
   fun map f arr = A.tabulate(A.length arr,(fn i => f(A.sub(arr,i))))
   fun mapi f arr = A.tabulate(A.length arr,(fn i => f(i, A.sub(arr,i))))
   open A
end

structure String = struct
   structure S = String
   fun space n =
     if n < 0 then raise (Fail "String.space: negative")
     else CharVector.tabulate(n, fn _ => #" ")
   fun whitespec #" " = true
     | whitespec #"\n" = true
     | whitespec #"\r" = true
     | whitespec #"\t" = true
     | whitespec #"\v" = true
     | whitespec _ = false
   fun losespecl sp s =
      let
         fun go n =
            if n >= size s
            then ""
            else
               if sp (CharVector.sub(s, n))
               then go (n + 1)
               else String.substring(s, n, size s - n)
      in
         go 0
      end
   open S
end

structure TextIO = struct
   structure T = TextIO
   (* Read lines from stdin.  No buffering. *)
   val lines : unit -> string array =
      let
         fun rmnl s =
            let
               val n = String.size s
            in
               if n = 0 then s else
                  if String.sub(s,n-1) = #"\n" then String.substring(s, 0, n-1) else s
            end
         fun lines acc =
            case T.inputLine T.stdIn of
               SOME l => lines (rmnl l :: acc)
             | NONE => rev acc
      in
         fn () => Array.fromList (lines [])
      end
   open T
end

structure Debug = struct
   val debug = ref false
   val print = fn l => if !debug then print (String.concat l) else ()
   fun printl l = print (l @ ["\n"])
end

structure Ref = struct
   fun incr (r as ref n) = r := n + 1
   fun decr (r as ref n) = r := n - 1
end
