module Main where

import Control.Exception (IOException, try)
import Control.Monad (forM_, unless)
import Data.List (isInfixOf)
import System.Directory (createDirectoryIfMissing, copyFile)
import System.Environment (getArgs)
import System.FilePath ((</>))
import ASyntax
import ECO.Backend (applyBackendPlan, checkBackendPorts)
import ECO.Interface (fixedPorts)
import ECO.Namespace (ecoNamespace, ecoEligible)
import ECO.Options
import ECO.Replay (fileIdentity, fiSHA256)
import ECO.Types
import ECO.Validate (validateGraph)
import Id (mk_homeless_id, isKeepId)
import IntLit (ilDec)
import Verilog

check label result = unless result (fail label)
reject label fragment action = do
    result <- try (action >> return ()) :: IO (Either IOException ())
    check label (either (isInfixOf fragment . show) (const False) result)

main = do
    [directory] <- getArgs
    namespaceTraversalTests
    createDirectoryIfMissing True directory
    let capture = directory </> "capture"
        preopt = directory </> "Safety.eco-preopt"
        planPath = directory </> "keep.plan"
        helper = mk_homeless_id "helper"
        definition = ADef helper (ATBit 1) (ASInt helper (ATBit 1) (ilDec 1)) []
        package = ASPackage (mk_homeless_id "Safety") False [] [] [] [] [] []
                    [definition] [] [] [] (ASPSignalInfo [] [] [] [] [] [] [] [] [] [])
                    (ASPCommentInfo [] [])
        node = EcoNode "def:helper" (Just "helper") "Bit:1" "definition"
                    ["rhs"] Nothing True [] False
        rhs = EcoNode "rhs" Nothing "Bit:1" "integer:1" [] Nothing False [] False
        graph nodes = EcoGraph 1 "Safety" "pre-aopt-typed-definitions" nodes ["helper"]
        base = graph [node,rhs]
        options = defaultECOOptions {ecoReplay=Just capture, ecoBackendPlan=Just planPath}
        apply g = do
            writeFile preopt (show g ++ "\n")
            ci <- fileIdentity capture
            pi <- fileIdentity preopt
            writeFile planPath (show (EcoBackendPlan 1 "Safety" (fiSHA256 ci)
                (fiSHA256 pi) [("helper","Bit:1","rhs")]) ++ "\n")
            applyBackendPlan options directory "Safety" g package
    writeFile capture "native plan API guard fixture\n"
    (kept, _) <- apply base
    check "pure helper should receive only keep annotation"
        (length (aspkg_values kept) == 1 && isKeepId (adef_objid (head (aspkg_values kept))) &&
         adef_expr (head (aspkg_values kept)) == adef_expr definition)
    copyFile planPath preopt
    before <- fileIdentity preopt
    reject "direct API overwrote its input plan" "preopt output would overwrite" $
        applyBackendPlan (options {ecoBackendPlan=Just preopt}) directory "Safety" base package
    after <- fileIdentity preopt
    check "rejected direct API output changed plan bytes" (after == before)
    forM_ [("undetermined", rhs {enOp="any"}),
           ("opaque", rhs {enOpaque=True}),
           ("cyclic", rhs {enOperands=["def:helper"]})] $ \(label,bad) ->
        reject (label ++ " cone accepted") "stale typed RHS" (apply (graph [node,bad]))
    let middle = rhs {enOperands=["opaque"]}
        opaque = rhs {enKey="opaque", enOpaque=True}
    reject "transitive opaque cone accepted" "stale typed RHS" (apply (graph [node,middle,opaque]))
    forM_ [("duplicate key", graph [node,rhs,rhs]),
           ("dangling operand", graph [node]),
           ("anonymous eligible", graph [node {enName=Nothing},rhs]),
           ("protected eligible", graph [node {enBoundary=Just "output"},rhs])] $ \(label,g) ->
        check (label ++ " graph accepted") (either (const True) (const False) (validateGraph g))
    let signal = mkVId "helper"
        input = mkVId "input"
        program body = VProgram [VModule (mkVId "Safety") []
                            [([VAInput input Nothing],[])]
                            (VMDecl (VVDecl VDReg Nothing [VVar signal]):body)] [] []
        assignment = VAssign (VLId signal) (VEConst 1)
        combinational body = VMStmt False (Valways (VAt (VEE (VEVar input)) body))
        allowed p = ecoEligible ["helper"] p == ["helper"]
        label n = VEWConst (mkVId (show n)) 1 2 n
        branch n = VCaseArm [label n] assignment
    check "complete combinational assignment excluded" (allowed (program [combinational assignment]))
    check "incomplete combinational assignment eligible"
        (not (allowed (program [combinational (Vif (VEVar input) assignment)])))
    check "clocked state eligible"
        (not (allowed (program [VMStmt False (Valways (VAt (VEEposedge (VEVar input)) assignment))])))
    check "exhaustive case excluded" (allowed (program [combinational (Vcase (VEVar input) [branch 0,branch 1] False False)]))
    check "incomplete case eligible" (not (allowed (program [combinational (Vcase (VEVar input) [branch 0] True True)])))
    let ordinary = program [combinational assignment]
    createDirectoryIfMissing True (directory </> "ordinary-control")
    writeFile (directory </> "ordinary-control" </> "Safety.eco-ports") (fixedPorts ordinary)
    checkBackendPorts options directory "Safety" ordinary
    let VProgram [m] dpi comment = ordinary
    reject "changed port accepted" "changed module ports" $
        checkBackendPorts options directory "Safety" (VProgram [m {vm_ports=[]}] dpi comment)
    putStrLn "PASS: pure-cone, graph-integrity, namespace and fixed-port guards"

-- Exercise names that appear outside ordinary expression leaves, including
-- original-register metadata carried by VId. No generic traversal is needed.
namespaceTraversalTests :: IO ()
namespaceTraversalTests = do
    let v = mkVId
        e = VEVar . v
        ranged n = Just (e n,VEConst 0)
        metadata = VMInst (v "Original") (v "original")
            (Right [(v "ORIG_PARAM",Just (e "origActual"))]) [(v "ORIG_PORT",Just (e "origWire"))]
        annotated n item = case v n of VId name source _ -> VId name source (Just item)
        expression = VEConcat
            [VEWConst (v "constantTag") 1 2 0,
             VEUnOp (v "unaryTag") VNot (e "operand"),
             VEOp (v "binaryTag") (e "left") VAdd (e "right"),
             VEIndex (v "array") (e "index"),
             VEFctCall (v "called") [VEVar (annotated "annotated" metadata)],e "scope.member"]
        candidates = ["helper","dpi","called","scope","reg","function","P","I","port_alias"]
        fixture = VProgram [VModule (v "Traversal") ["ignored comment"]
            [([VAInout (v "port") (Just (v "port_alias")) (Just (ranged "port_bound")),
               VAParameter (v "parameter") (ranged "parameter_bound") (e "parameter_default")],[])]
            [VMDecl (VVDecl VDWire (ranged "decl_bound") (map (VVar . v) candidates)),
             VMDecl (VVDecl VDReg Nothing [VArray (e "array_bound",VEConst 0) (v "array")]),
             VMInst (v "Child") (v "child") (Right [(v "P",Just (e "actualParam"))])
                 [(v "I",Just (e "actualWire"))],
             VMInst (v "PosChild") (v "posChild") (Left [(Nothing,e "posActual")]) [],
             VMAssign (VLConcat [VLId (v "helper"),VLSub (VLId (v "array")) (e "index")]) expression,
             VMGroup False [[VMComment [] (VMRegGroup (v "regGroup") "" []
                 (VMDecl (VVDWire Nothing (VVar (v "reg")) (e "regValue"))))]],
             VMFunction (VFunction (v "function") (ranged "function_bound")
                 [VVDecl VDInput Nothing [VVar (v "function_input")]]
                 (VAssign (VLId (v "function")) (e "function_input")))]]
            [VDPI (v "dpi") VDT_void [(v "dpi_arg",True,VDT_int)]] []
        expected = candidates ++ ["Traversal","port","port_bound","parameter","parameter_bound",
            "parameter_default","decl_bound","array_bound","array","Child","child","actualParam",
            "actualWire","PosChild","posChild","posActual","index","constantTag","unaryTag",
            "operand","binaryTag","left","right","annotated","Original","original","ORIG_PARAM",
            "origActual","ORIG_PORT","origWire","scope.member","regGroup","regValue",
            "function_bound","function_input","dpi_arg"]
    check "typed traversal omitted a reserved identifier" (all (`elem` ecoNamespace fixture) expected)
    check "typed traversal admitted a protected identity" (ecoEligible candidates fixture == ["helper"])
    let hidden = v "hidden"
        output = v "output"
        metaWrite = VMStmt False (VAssign (VLId hidden) (VEConst 0))
        tagged = VEVar (annotated "tag" metaWrite)
        assign = VAssign (VLId output) (VEConst 1)
        program body = VProgram [VModule (v "Metadata") [] []
            [VMDecl (VVDecl VDReg Nothing [VVar hidden,VVar output]),
             VMStmt False (Valways (VAt (VEE (e "input")) body))]] [] []
    forM_ [("rhs",VAssign (VLId output) tagged),
           ("condition",Vifelse tagged assign assign),
           ("selector",Vcase tagged [VDefault assign] False False),
           ("case label",Vcase (e "input") [VCaseArm [tagged] assign,VDefault assign] False False),
           ("task argument",VSeq [VTask (v "$display") [tagged],assign]),
           ("assert event",VSeq [VAssert (VEE tagged) [],assign])] $ \(label,body) ->
        check ("metadata write lost protection in " ++ label)
            (ecoEligible ["hidden","output"] (program body) == ["output"])
