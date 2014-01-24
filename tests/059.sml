
val _ =
   let in
      assert "initial" (initialRule(db, rid))
    ; (case H.find ruleChildren rid of
          NONE => ()
        | SOME (Left (ref seqs)) =>
          if Seq.Id.Set.isEmpty seqs then 6 else 7)
   end
