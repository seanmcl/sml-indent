

signature Term = sig
   include Comparable where type comparable = t
   include Showable where type showable = t
end
