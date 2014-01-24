

structure Filter : sig
   datatype t = T of
      {

end

structure Foci : sig
   datatype t = T of
      { seqs: (Seq.t * SC.t) list
      , rules: Rule.Input.t list
      , goal: Seq.t
      , global: Pred.set }
end

