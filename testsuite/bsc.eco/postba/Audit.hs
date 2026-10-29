module Main where
import Control.Monad (unless)
import ECO.Types
import ECOMatch
import ECOAggressive
import ECOAudit

node key name ty = EcoNode key (Just name) ty "integer:1" [] Nothing True [] False
graph nodes reserved = EcoGraph 1 "m" "final" nodes reserved
old = graph [node "oa" "a" "Bit:8",node "ob" "b" "Bit:8",node "op" "port" "Bit:1"] ["a","b","port"]
new = graph [node "nx" "x" "Bit:8",node "ny" "y" "Bit:2"] ["x","y","port"]
need label ok = unless ok (error label)
bad label result = need label (case result of Left _ -> True; _ -> False)
main = do
    let Right (matches,Just audit) = matchGraphsWithAudit Aggressive old new
        Right names = proposeRenames (egReserved new) old new matches
        check = checkAssignmentAudit old new
        Right checked = check audit names
    need "complete cardinality/protected/surplus accounting"
        (checkedOld checked == 3 && checkedNew checked == 2 && checkedPairs checked == 1 && checkedProtected checked == 1)
    bad "missing row" (check (audit {auditNew=tail (auditNew audit)}) names)
    bad "duplicate row" (check (audit {auditNew=head (auditNew audit):auditNew audit}) names)
    bad "missing component" (check (audit {auditGroups=tail (auditGroups audit)}) names)
    bad "false cardinality" (check (audit {auditGroups=map (\g -> g {groupMatched=99}) (auditGroups audit)}) names)
    bad "unexplained disposition" (check (audit {auditNew=map (\d -> d {dispositionEvidence=""}) (auditNew audit)}) names)
    bad "changed counterpart" (check (audit {auditNew=map (\d -> if dispositionStatus d=="matched" then d {dispositionCounterpart=Just "op"} else d) (auditNew audit)}) names)
    bad "noneligible rename" (check audit (("port","illegal"):names))
    bad "duplicate rename" (check audit (names++names))
    bad "protected output" (check audit [("x","port"),("y","fresh")])
    bad "surplus steals old name" (check audit [("x","a"),("y","b")])
    bad "historical allocation omitted" (check audit [])
    putStrLn "PASS: 12 independent assignment audit checks"
