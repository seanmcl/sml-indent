
val removeRule : db * Rule.id -> unit =
   fn (db as D {rules, seqChildren, ruleChildren, ...}, rid) =>
      let in
         checkDbConsistency db
       ; assert "should be member" (H.inDomain rules rid)
       ; assert "should be member" (H.inDomain ruleChildren rid)
       (* remove from seqChildren and ruleChildren *)
       ; case ruleParents (db, rid) of
            P.Derived (pqid, prid) =>
            let in
               if seqAlive(db, pqid)
               then case H.find seqChildren pqid of
                       NONE => raise Impossible
                     | SOME (_, rules) => rules := Rule.Id.Set.delete(!rules, rid)
               else ()
             ; if ruleAlive(db, prid)
               then case H.find ruleChildren prid of
                       SOME (Right rules) => rules := Rule.Id.Set.delete(!rules, rid)
                     | _ => raise Impossible
               else ()
            end
          | _ => assert "initial" (initialRule(db, rid))
       (* check to be sure it has no children *)
       ; case H.find ruleChildren rid of
            NONE => ()
          | SOME (Left (ref seqs)) =>
            if Seq.Id.Set.isEmpty seqs
            then ()
            else raise Fail "Error: This node has children"
          | SOME (Right (ref rules)) =>
            if Rule.Id.Set.isEmpty rules
            then ()
            else raise Fail "Error: This node has children"
       (* remove from ruleChildren *)
       ; ignore(H.remove ruleChildren rid)
       (* kill the rule *)
       ; killRule(db, rid)
       ; checkDbConsistency db
      end
