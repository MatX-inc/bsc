module Main where
import Control.Monad (unless)
import ECORename
import Verilog

v = mkVId
wire n = VMDecl (VVDecl VDWire Nothing [VVar (v n)])
reg n = VMDecl (VVDecl VDReg Nothing [VVar (v n)])
prog = VProgram [VModule (v "top") [] [([VAInput (v "input") Nothing,
    VAOutput (v "result") Nothing], [])]
    [wire "a", wire "b", reg "state", VMAssign (VLId (v "a")) (VEVar (v "input")),
     VMAssign (VLId (v "b")) (VEVar (v "a")),
     VMAssign (VLId (v "result")) (VEVar (v "b")),
     VMInst (v "Child") (v "child") (Right []) [(v "a", Just (VEVar (v "b")))],
     VMStmt False (VAssignA (VLId (v "state")) (VEVar (v "a")))]] [] []
check True _ = return ()
check False msg = error msg
reject name pairs = case renameInternalSignals ["a","b","state","input","child"] pairs prog of
    Left _ -> return ()
    Right _ -> error ("accepted " ++ name)
main = do
    -- The formal named a reserves a: b remains eligible, a is conservatively
    -- protected even though its local occurrences could otherwise be renamed.
    check (ecoEligible ["a","b","state","input","child"] prog == ["b"]) "eligibility"
    reject "state" [("state","old")]
    reject "port" [("input","old")]
    reject "formal collision" [("b","a")]
    reject "duplicate target" [("b","old"),("b","old")]
    reject "hierarchy" [("b","old.member")]
    reject "reserved" [("b","module")]
    case renameInternalSignals ["b"] [("b","old")] prog of
      Left e -> error e
      Right p -> do
        check ("old" `elem` ecoNamespace p) "renamed declaration"
        check (not ("b" `elem` ecoNamespace p)) "renamed references"
    let swapProg = VProgram [VModule (v "top") [] [] [wire "a", wire "b",
                     VMAssign (VLId (v "a")) (VEVar (v "b"))]] [] []
    case renameInternalSignals ["a","b"] [("a","b"),("b","a")] swapProg of
      Right (VProgram [m] _ _) -> check (last (vm_body m) ==
         VMAssign (VLId (v "b")) (VEVar (v "a"))) "simultaneous cycle"
      Left e -> error e
      _ -> error "shape changed"
    let assignTemp = VAssign (VLId (v "temp")) (VEVar (v "input"))
        procedural body = VProgram [VModule (v "top") [] []
            [reg "temp", VMDecl (VVDecl VDWire (Just (VEConst 7,VEConst 0)) [VVar (v "bus")]), VMDecl (VVDecl VDWire (Just (VEConst 1,VEConst 0)) [VVar (v "sel")]), VMStmt False (Valways (VAt (VEE (VEVar (v "input"))) body))]] [] []
        binaryCase = Vcase (VEVar (v "sel"))
            [VCaseArm [VEWConst (v "lit") 2 10 n] assignTemp | n <- [0..3]] False False
        withDefault = Vcase (VEVar (v "sel"))
            [VCaseArm [VEConst 0] assignTemp, VDefault assignTemp] False False
    let sliceCase = binaryCase { vs_case_expr = VESelect (VEVar (v "bus")) (VEConst 6) (VEConst 5) }
        badSlice = binaryCase { vs_case_expr = VESelect (VEVar (v "bus")) (VEConst 8) (VEConst 7) }
        dynamicSlice = binaryCase { vs_case_expr = VESelect (VEVar (v "bus")) (VEVar (v "sel")) (VEConst 0) }
        bitCase = Vcase (VESelect1 (VEVar (v "bus")) (VEConst 7))
            [VCaseArm [VEWConst (v "lit") 1 10 n] assignTemp | n <- [0..1]] False False
    let nonzeroRange = VProgram [VModule (v "top") [] []
            [reg "temp", VMDecl (VVDecl VDWire (Just (VEConst 8,VEConst 1)) [VVar (v "bus")]),
             VMStmt False (Valways (VAt (VEE (VEVar (v "bus"))) sliceCase))]] [] []
    check (null (ecoEligible ["temp"] nonzeroRange)) "unsupported nonzero-based indexed selector"
    check (ecoEligible ["temp"] (procedural sliceCase) == ["temp"]) "binary complete constant slice case"
    check (ecoEligible ["temp"] (procedural bitCase) == ["temp"]) "binary complete constant bit case"
    check (null (ecoEligible ["temp"] (procedural badSlice))) "out-of-range slice"
    check (null (ecoEligible ["temp"] (procedural dynamicSlice))) "dynamic slice width"
    check (ecoEligible ["temp"] (procedural binaryCase) == ["temp"]) "binary complete case has don't-care X/Z default"
    let partialCase = Vcase (VEVar (v "sel"))
            [VCaseArm [VEWConst (v "lit") 2 10 n] assignTemp | n <- [0..2]] True True
        duplicateCase = Vcase (VEVar (v "sel"))
            [VCaseArm [VEWConst (v "lit") 2 10 n] assignTemp | n <- [0,1,2,2]] True True
        extraDuplicateCase = Vcase (VEVar (v "sel"))
            [VCaseArm [VEWConst (v "lit") 2 10 n] assignTemp | n <- [0,1,2,3,3]] False False
        groupedCase = Vcase (VEVar (v "sel"))
            [VCaseArm [VEWConst (v "lit") 2 10 n | n <- [0..3]] assignTemp] False False
        missingAssignment = Vcase (VEVar (v "sel"))
            [VCaseArm [VEWConst (v "lit") 2 10 n]
                (if n == 3 then VAssign (VLId (v "other")) (VEConst 0) else assignTemp) | n <- [0..3]] False False
        wrongWidthCase = Vcase (VEVar (v "sel"))
            [VCaseArm [VEWConst (v "lit") 3 10 n] assignTemp | n <- [0..3]] False False
    check (null (ecoEligible ["temp"] (procedural partialCase))) "missing binary arm despite full_case hint"
    check (null (ecoEligible ["temp"] (procedural duplicateCase))) "duplicate label does not establish coverage"
    check (null (ecoEligible ["temp"] (procedural missingAssignment))) "all binary arms must assign destination"
    check (null (ecoEligible ["temp"] (procedural extraDuplicateCase))) "reject duplicated complete binary domain"
    check (null (ecoEligible ["temp"] (procedural wrongWidthCase))) "unsupported case label width"
    check (ecoEligible ["temp"] (procedural groupedCase) == ["temp"]) "grouped binary labels"
    check (ecoEligible ["temp"] (procedural withDefault) == ["temp"]) "complete case"
    check (null (ecoEligible ["temp"] (procedural (Vif (VEVar (v "sel")) assignTemp)))) "incomplete if"
    check (ecoEligible ["temp"] (procedural (VSeq [assignTemp, Vif (VEVar (v "sel")) assignTemp])) == ["temp"])
        "unconditional procedural default"
    case renameInternalSignals ["temp"] [("temp","old")] (procedural binaryCase) of
      Left e -> error e
      Right p -> check ("old" `elem` ecoNamespace p && not ("temp" `elem` ecoNamespace p))
          "renamed BSC combinational case temporary"
    putStrLn "ECO renaming tests passed"
