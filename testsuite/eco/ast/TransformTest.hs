module Main where

import ECOASTTransform hiding (generateASTCandidates)
import qualified ECOASTTransform as T
import ECOASTLexical
import Verilog
import VLocalNames (mapRenameableVIds)
import qualified Data.Map.Strict as M
import Data.Either (isRight)
import Data.List (find, isInfixOf)
import PPrint (pp80)
import Control.Monad (unless)

-- Every generated candidate must pass the lexical and declaration gates the
-- search applies before scoring; the whole suite runs through this check.
emittable p=isRight (lexicallyEmittable p >> declaredBeforeUse p)
gated cs=case [(acFeature c,acDescription c,acOperations c,r) | c<-cs, Left r<-[lexicallyEmittable (acProgram c) >> declaredBeforeUse (acProgram c)]] of
  [] -> cs
  bad -> error ("generated candidate is not emittable: " ++ show bad)
generateASTCandidates fs n a b=gated (T.generateASTCandidates fs n a b)
everything=generateASTCandidates (map fst featureTable) 128

v=mkVId
e = VEVar . v
w n x=VEWConst defaultVId n 10 x
r n=if n==1 then Nothing else Just (VEConst (n-1),VEConst 0)
wire n x=VMDecl (VVDecl VDWire (r n) [VVar (v x)])
reg n x=VMDecl (VVDecl VDReg (r n) [VVar (v x)])
assign n x=VMAssign (VLId (v n)) x
op a o b=VEOp defaultVId a o b
program xs=VProgram [VModule (v "M") []
  [([VAInput (v "a") (r 8),VAInput (v "b") (r 8),VAInput (v "c") Nothing,
     VAOutput (v "o") (r 8)],[])] xs] [] ["NEW compiler header"]
body (VProgram [m] _ _)=vm_body m
body _=error "fixture program"
cs f a b=generateASTCandidates [f] 128 a b
check label ok=unless ok (error label)
hasItem x c=x `elem` body (acProgram c)
flatten=concatMap go
 where go (VMGroup _ xss)=flatten (concat xss)
       go (VMComment _ x)=go x
       go (VMRegGroup _ _ _ x)=go x
       go x=[x]
items c=flatten (body (acProgram c))
get f xs=case find f xs of Just x->x; _->error "expected proposal absent"
mux out selector defaultArm=VMStmt False (Valways (VAt (VEE (e selector))
   (Vcase (e selector) ([VCaseArm [w 1 0] (VAssign (VLId (v out)) (e "a")),
                        VCaseArm [w 1 1] (VAssign (VLId (v out)) (e "b"))]
                       ++if defaultArm then [VDefault (VAssign (VLId (v out)) (w 8 0))] else []) False False)))

main=do
  let old=program [wire 8 "x",assign "x" (e "a"),assign "o" (e "x")]
      new=program [wire 8 "x",assign "o" (e "x"),assign "x" (e "b")]
      layout=get ((=="F04").acFeature) (cs "F04" old new)
  check "layout retains new function" (body (acProgram layout)==[wire 8 "x",assign "x" (e "b"),assign "o" (e "x")])
  let inst=VMInst (v "Child") (v "child") [] [(v "A",Just (e "x")),(v "Y",Just (e "y"))]
      oldI=program [wire 8 "x",wire 8 "y",assign "x" (e "a"),inst,assign "o" (e "y")]
      newI=program [wire 8 "x",wire 8 "y",assign "o" (e "y"),inst,assign "x" (e "b")]
      mixed=get ((=="F04").acFeature) (cs "F04" oldI newI)
  check "mixed-member anchors retain new child identity" (body (acProgram mixed)==[wire 8 "x",wire 8 "y",assign "x" (e "b"),inst,assign "o" (e "y")])
  let oldA=program [wire 1 "s",wire 1 "t",reg 8 "q",assign "s" (VESelect1 (e "a") (VEConst 0)),
                    assign "t" (e "c"),mux "q" "s" True,assign "o" (e "q")]
      newA=program [wire 1 "t",reg 8 "q",assign "t" (e "c"),mux "q" "t" True,assign "o" (e "q")]
      restored=get (elem (assign "s" (e "t")).items) (cs "F07" oldA newA)
  check "restored alias computes new target" (assign "s" (e "t") `elem` items restored)
  check "restored alias did not copy old function" (assign "s" (VESelect1 (e "a") (VEConst 0)) `notElem` items restored)
  check "restored role is declared in the declaration partition before its use"
    (body (acProgram restored)==[wire 1 "t",reg 8 "q",wire 1 "s",assign "t" (e "c"),mux "q" "s" True,assign "o" (e "q"),assign "s" (e "t")])
  let oldR=program [reg 8 "q",reg 8 "z",mux "q" "c" True,mux "z" "c" True,
                    assign "o" (VEConcat [VESelect (e "q") (VEConst 3) (VEConst 0),
                                         VESelect (e "z") (VEConst 3) (VEConst 0)])]
      newR=program [reg 8 "q",mux "q" "c" True,
                    assign "o" (VEConcat [VESelect (e "q") (VEConst 3) (VEConst 0),
                                         VESelect (e "q") (VEConst 3) (VEConst 0)])]
      clone=get (any ((=="selective-pure-process-clone").fst).acOperations) (cs "F07" oldR newR)
      expected=assign "o" (VEConcat [VESelect (e "q") (VEConst 3) (VEConst 0),VESelect (e "z") (VEConst 3) (VEConst 0)])
  check "clone redirects only historical slot" (expected `elem` items clone)
  check "cloned state is declared beside the existing declarations"
    (body (acProgram clone)==[reg 8 "q",reg 8 "z",mux "q" "c" True,expected,mux "z" "c" True])
  check "explicit default clone supports strict" (not (acBinaryOnly clone))
  let oldB=program [reg 8 "q",reg 8 "z",mux "q" "c" False,mux "z" "c" False,assign "o" (e "z")]
      newB=program [reg 8 "q",mux "q" "c" False,assign "o" (e "q")]
      binary=get (any ((=="selective-pure-process-clone").fst).acOperations) (cs "F07" oldB newB)
  check "exhaustive no-default clone is binary-only" (acBinaryOnly binary)
  let incomplete=VMStmt False (Valways (VAt (VEE (e "c")) (Vif (e "c") (VAssign (VLId (v "q")) (e "a")))))
      bad=program [reg 8 "q",incomplete,assign "o" (e "q")]
  check "incomplete clone refused" (null (cs "F07" oldB bad))
  let oldT=program [assign "o" (VEIf (e "c") (e "a") (e "b"))]
      newT=program [assign "o" (e "a")]
      template=get ((=="F10").acFeature) (cs "F10" oldT newT)
  check "equal arm template preserves current value" (assign "o" (VEIf (e "c") (e "a") (e "a")) `elem` items template)
  let oldF=program [wire 1 "eq",assign "eq" (op (e "a") VEQ (e "b")),assign "o" (VEIf (op (e "c") VLOr (e "eq")) (w 8 1) (w 8 0))]
      newF=program [wire 1 "eq",assign "eq" (op (e "a") VEQ (e "b")),assign "o" (VEIf (op (e "eq") VLAnd (e "c")) (w 8 1) (w 8 0))]
      forms=cs "F11" oldF newF
  check "bounded Boolean alternatives generated" (length forms>=6)
  check "Boolean alternatives explicitly binary" (all acBinaryOnly forms)
  let wideOld=program [wire 7 "x",assign "x" (e "a"),assign "o" (e "x")]
      wideNew=program [assign "o" (e "a")]
  check "truncating role extraction refused" (null (cs "F07" wideOld wideNew))
  let sy=M.fromList [("a",(VDInput,r 8)),("b",(VDInput,r 8))]
  check "out of range select unknown" (expressionWidth sy (VESelect (e "a") (VEConst 8) (VEConst 0))==Nothing)
  check "signed/foreign calls not width evidence" (expressionWidth sy (VEFctCall (v "$signed") [e "a"])==Nothing)
  check "deterministic proposals" (generateASTCandidates ["F04","F07","F11"] 16 oldA newA==generateASTCandidates ["F04","F07","F11"] 16 oldA newA)
  check "bounded proposal list" (length (generateASTCandidates ["F11"] 1 oldF newF)==1)
  let oldArray=program [VMDecl (VVDecl VDReg (r 8) [VArray (VEConst 3,VEConst 0) (v "mem")]),assign "o" (e "a")]
      newArray=program [VMDecl (VVDecl VDReg (r 8) [VArray (VEConst 7,VEConst 0) (v "mem")]),assign "o" (e "b")]
  check "array shape protected" (null (cs "F04" oldArray newArray))
  let oldGuard=program [wire 8 "x",VMGroup True [[assign "x" (e "a")]],assign "o" (e "x")]
      newGuard=program [wire 8 "x",assign "o" (e "x"),assign "x" (e "b")]
  check "translate context protected" (null (cs "F04" oldGuard newGuard))
  let oldPragma=program [wire 8 "x",VMComment ["synthesis keep"] (assign "x" (e "a")),assign "o" (e "x")]
  check "directive comments not copied" (null (cs "F04" oldPragma new))
  let oldInline=program [assign "o" (VEIf (op (e "a") VUGT (e "b")) (w 8 1) (w 8 0))]
      newInline=program [wire 1 "shared",assign "shared" (op (e "a") VULE (e "b")),
                         assign "o" (VEIf (VEUnOp defaultVId VNot (e "shared")) (w 8 1) (w 8 0))]
      bundled=generateASTCandidates ["F08","F11"] 128 oldInline newInline
      inline=get (elem (assign "o" (VEIf (op (e "a") VUGT (e "b")) (w 8 1) (w 8 0))).items) bundled
  check "inline and complement bundle binary" (acBinaryOnly inline)
  check "inline removes exact-width helper" (wire 1 "shared" `notElem` items inline)
  let narrow=program [wire 7 "short",assign "short" (e "a"),assign "o" (e "short")]
  check "inline truncation refused" (null (cs "F08" oldInline narrow))
  let child=VMInst (v "Child") (v "sameChild") [] [(v "A",Just (e "shared"))]
  check "inline child connection protected" (null (cs "F08" oldInline (program (body newInline++[child]))))
  let ca x=VCaseArm [w 1 x] (VAssign (VLId (v "q")) (e "a"))
      cd=VDefault (VAssign (VLId (v "q")) (e "a"))
      caseM arms=program [reg 8 "q",VMStmt False (Valways (VAt (VEE (e "c")) (Vcase (e "c") arms False False))),assign "o" (e "q")]
      shaped=cs "F09" (caseM [VCaseArm [w 1 0,w 1 1] (VAssign (VLId (v "q")) (e "a")),cd]) (caseM [ca 0,cd])
  check "explicit default case skeleton proposed" (not (null shaped))
  check "missing default not invented" (null (cs "F09" (caseM [ca 0,ca 1]) (caseM [ca 0])))
  let conditionalOldRole=program [assign "q" (VEIf (e "c") (e "a") (e "b")),assign "o" (e "q")]
  check "complete cases are never lowered to continuous conditionals by any registered feature"
    (all (all ((/="case-to-conditional").fst).acOperations) (everything conditionalOldRole (caseM [ca 0,VCaseArm [w 1 1] (VAssign (VLId (v "q")) (e "b"))])))
  let self=VMStmt False (Valways (VAt (VEE (e "c")) (Vifelse (e "c") (VAssign (VLId (v "q")) (e "q")) (VAssign (VLId (v "q")) (e "b")))))
  check "self-dependent process not cloned" (null (cs "F07" oldB (program [reg 8 "q",self,assign "o" (e "q")])))
  let blockA=mux "q" "c" True; blockB=mux "z" "c" True
      po=program [reg 8 "q",reg 8 "z",blockA,blockB,assign "o" (e "q")]
      pn=program [reg 8 "q",reg 8 "z",blockB,blockA,assign "o" (e "q")]
  check "process order native objects" (any ((==body po).body.acProgram) (cs "F03" po pn))
  let event selector=Valways (VAt (VEEOr (VEE (e selector)) (VEE (e "tsu$globalRf")))
                             (VAssign (VLId (v "q")) (e "a")))
      longName=replicate 90 's'
  check "native event printing uses current short names" ("always@(x or tsu$globalRf)" `isInfixOf` pp80 (event "x"))
  check "long event actually wraps" (not (("always@("++longName++" or tsu$globalRf)") `isInfixOf` pp80 (event longName)))
  let dcOld=program [assign "o" (e "a")]
      dcNew=program [wire 1 "enable",assign "enable" (w 1 1),
                     assign "o" (VEIf (e "enable") (e "a") (VEUnknown 8 "X"))]
      zNew=program [assign "o" (VEIf (e "c") (e "a") (VETriConst (replicate 8 Vz)))]
  check "no feature treats an explicit X or Z arm as dont care"
    (all (\new->all ((/=[assign "o" (e "a")]).body.acProgram) (everything dcOld new)) [dcNew,zNew])
  let edge rhs=VMStmt False (Valways (VAt (VEEposedge (e "c")) (VAssignA (VLId (v "q")) rhs)))
      stateOld=program [reg 8 "q",edge (op (e "a") VAdd (e "b")),assign "o" (e "q")]
      stateNew=program [reg 8 "q",wire 8 "d",assign "d" (op (e "a") VAdd (e "b")),edge (e "d"),assign "o" (e "q")]
  check "sequential RHS helpers are never inlined by any registered feature"
    (all (all ((/="inline-sequential-rhs").fst).acOperations) (everything stateOld stateNew))
  let stateNarrow=program [reg 8 "q",wire 7 "d",assign "d" (e "a"),edge (e "d"),assign "o" (e "q")]
  check "state helper truncation rejected" (null (cs "F08" stateOld stateNarrow))
  let initial=VMStmt False (Vinitial (VAssign (VLId (v "q")) (e "d")))
  check "initial dependency blocks state substitution" (null (cs "F08" stateOld (program (body stateNew++[initial]))))
  let control=VMStmt False (Valways (VAt (VEEposedge (e "c")) (Vif (e "en") (VAssignA (VLId (v "q")) (e "a")))))
      controlNew=program [reg 8 "q",wire 1 "en",assign "en" (w 1 1),control,assign "o" (e "q")]
  check "sequential control helpers stay fixed" (null (cs "F08" stateOld controlNew))
  check "default retains new register input and enable scaffolding" (null (cs "F08" stateOld stateNew))
  let unused=program [wire 8 "novel",assign "novel" (e "a"),assign "o" (e "b")]
  check "default retains novel unused helper scaffolding" (null (cs "F08" (program [assign "o" (e "a")]) unused))
  let historicalSum=program [assign "o" (op (e "a") VAdd (e "b"))]
      extractedSum=program [wire 8 "sum",assign "sum" (op (e "a") VAdd (e "b")),assign "o" (e "sum")]
      recoveredSum=get (elem (assign "o" (op (e "a") VAdd (e "b"))).items) (cs "F08" historicalSum extractedSum)
  check "historical expression shape justifies helper recovery" (wire 8 "sum" `notElem` items recoveredSum)
  let aliasState=program [reg 8 "q",edge (e "a"),wire 8 "alias",assign "alias" (e "q"),assign "o" (e "alias")]
  check "old output name does not justify novel state alias inlining" (null (cs "F08" (program [assign "o" (e "a")]) aliasState))
  let slicedState=program [reg 16 "wideState",wire 8 "slice",assign "slice" (VESelect (e "wideState") (VEConst 7) (VEConst 0)),assign "o" (e "slice")]
  check "old output name does not justify novel state slice inlining" (null (cs "F08" (program [assign "o" (e "a")]) slicedState))
  let novelConsumer=program (body extractedSum++[wire 8 "newRole",assign "newRole" (e "sum")])
  check "historical consumer does not license compacting novel consumer" (null (cs "F08" historicalSum novelConsumer))
  let oldInitialized=program (body historicalSum++[wire 8 "existing",assign "existing" (e "a")])
      newInitialized=program (body extractedSum++[VMDecl (VVDWire (r 8) (VVar (v "existing")) (e "sum"))])
  check "initialized wire consumer participates in historical shape accounting" (null (cs "F08" oldInitialized newInitialized))
  let differentOperator=program [wire 8 "sum",assign "sum" (op (e "a") VSub (e "b")),assign "o" (e "sum")]
  check "historical destination cannot hide different operator structure" (null (cs "F08" historicalSum differentOperator))
  let newStateOperand=program [reg 8 "q",wire 8 "sum",assign "sum" (op (e "q") VAdd (e "b")),assign "o" (e "sum")]
  check "historical operators cannot hide novel state operand" (null (cs "F08" historicalSum newStateOperand))
  let shellOld=program [assign "o" (VEIf (e "c") (w 8 0) (e "a"))]
      changedShellGuard=op (e "c") VLOr (op (e "b") VEQ (w 8 3))
      shellNew predicate yes no extra=program (extra++[wire 8 "branch",
        assign "branch" (VEIf predicate yes no),assign "o" (e "branch")])
      shellGood=shellNew changedShellGuard (w 8 0) (e "a") []
      shellRecovered=get (elem (assign "o" (VEIf changedShellGuard (w 8 0) (e "a"))).items)
                       (cs "F08" shellOld shellGood)
  check "historical conditional shell permits changed pure guard" (wire 8 "branch" `notElem` items shellRecovered)
  check "conditional shell rejects new state guard" (null (cs "F08" shellOld
    (shellNew (e "newState") (w 8 0) (e "a") [reg 1 "newState"])))
  check "conditional shell rejects changed data arm" (null (cs "F08" shellOld
    (shellNew changedShellGuard (w 8 1) (e "a") [])))
  check "conditional shell rejects new leaf guard" (null (cs "F08" shellOld
    (shellNew (e "unknownLeaf") (w 8 0) (e "a") [])))
  let shellWithOldName=program [wire 1 "oldGuard",assign "oldGuard" (e "c"),assign "o" (VEIf (e "c") (w 8 0) (e "a"))]
  check "conditional shell rejects new state hidden behind old name" (null (cs "F08" shellWithOldName
    (shellNew (e "oldGuard") (w 8 0) (e "a") [reg 1 "newState",wire 1 "oldGuard",assign "oldGuard" (e "newState")])) )
  let relationValue rel a b=VEConcat [w 7 0,op a rel b]
      relationOld=program [assign "o" (relationValue VULT (e "a") (e "b"))]
      relationNew a b extra=program (extra++[wire 1 "cmp",assign "cmp" (op a VULE b),assign "o" (VEConcat [w 7 0,e "cmp"])])
      relationGood=relationNew (e "a") (e "b") []
      relationRecovered=get (elem (assign "o" (relationValue VULE (e "a") (e "b"))).items)
        (cs "F08" relationOld relationGood)
  check "historical comparison shell retains new relation over exact operands" (wire 1 "cmp" `notElem` items relationRecovered)
  check "comparison shell rejects new operand" (null (cs "F08" relationOld (relationNew (e "b") (e "b") [])))
  check "comparison shell rejects new state operand" (null (cs "F08" relationOld (relationNew (e "newState") (e "b") [reg 8 "newState"])))
  let changedArithmetic=program [wire 8 "sum",assign "sum" (op (e "a") VSub (e "b")),assign "o" (e "sum")]
  check "relation family permission does not permit arithmetic family changes" (null (cs "F08" historicalSum changedArithmetic))
  let aliasBody width spelling=[reg width "pointer",wire width spelling,assign spelling (e "pointer"),
        assign "o" (VEConcat [w (8-width) 0,e spelling])]
      aliasOld=program (aliasBody 2 "oldAlias")
      aliasNew=program (aliasBody 3 "newAlias")
      aliasRecovered=get (const True) (cs "F01" aliasOld aliasNew)
  check "width-grown register read alias recovers old name only"
    (body (acProgram aliasRecovered)==aliasBody 3 "oldAlias")
  check "width alias rejects changed register identity"
    (null (cs "F01" aliasOld (program [reg 3 "other",wire 3 "newAlias",assign "newAlias" (e "other"),assign "o" (VEConcat [w 5 0,e "newAlias"])])))
  check "width alias rejects ambiguous same-register aliases"
    (null (cs "F01" aliasOld (program (aliasBody 3 "newAlias"++[wire 3 "another",assign "another" (e "pointer")]))))
  check "width alias rejects occupied historical target"
    (null (cs "F01" aliasOld (program (aliasBody 3 "newAlias"++[wire 3 "oldAlias",assign "oldAlias" (e "a")]))))
  check "width alias rejects truncated register read"
    (null (cs "F01" aliasOld (program [reg 3 "pointer",wire 2 "newAlias",assign "newAlias" (e "pointer"),assign "o" (VEConcat [w 6 0,e "newAlias"])])))
  let arm label value=VCaseArm [label] (VAssign (VLId (v "q")) value)
      defaultArm value=VDefault (VAssign (VLId (v "q")) value)
      oldDefault=caseM [arm (w 1 0) (e "a"),defaultArm (e "b")]
      newFull=caseM [arm (w 1 0) (e "a"),arm (w 1 1) (e "b")]
      restoredDefault=get (any ((=="binary-case-default").fst).acOperations) (cs "F09" oldDefault newFull)
  check "historical default recovery explicitly binary" (acBinaryOnly restoredDefault)
  check "default recovery preserves current arms" (body (acProgram restoredDefault)==body oldDefault)
  check "default recovery rejects incomplete selector domain" (null (cs "F09" oldDefault (caseM [arm (w 1 0) (e "a")])))
  check "default recovery rejects duplicate labels" (null (cs "F09" oldDefault (caseM [arm (w 1 0) (e "a"),arm (w 1 0) (e "b")])))
  check "default recovery rejects wrong-width labels" (null (cs "F09" oldDefault (caseM [arm (w 2 0) (e "a"),arm (w 2 1) (e "b")])))
  let left=op (e "a") VAdd (w 8 1)
      right=op (e "b") VAdd (w 8 1)
      helpers=[wire 8 "left",wire 8 "right",assign "left" left,assign "right" right]
      prior=program (helpers++[assign "o" (VEIf (e "c") (e "left") (e "right"))])
      expanded=program (helpers++[assign "o" right])
      reused=get (elem (assign "o" (VEIf (e "c") (e "right") (e "right"))).items) (cs "F10" prior expanded)
  check "conditional template reuses surviving historical helper" (not (acBinaryOnly reused))
  let narrowed=program [wire 8 "left",wire 7 "right",assign "left" left,assign "right" right,assign "o" right]
  check "historical helper truncation not hidden by re-extraction"
    (all (not.elem (assign "o" (VEIf (e "c") (e "right") (e "right"))).items) (cs "F10" prior narrowed))
  let cyclic=program [wire 8 "left",wire 8 "right",assign "left" left,assign "right" (e "right"),assign "o" right]
  check "cyclic helper not reused by conditional template"
    (all (not.elem (assign "o" (VEIf (e "c") (e "right") (e "right"))).items) (cs "F10" prior cyclic))
  let changed=program (helpers++[assign "o" (op (e "b") VAdd (w 8 2))])
  check "different functional value not assigned historical helper"
    (all (not.elem (assign "o" (VEIf (e "c") (e "right") (e "right"))).items) (cs "F10" prior changed))
  let eqGuard=op (e "a") VEQ (e "b")
      neGuard=op (op (e "a") VULT (e "b")) VLOr (op (e "a") VUGT (e "b"))
      conditionalOld=program [assign "o" (VEIf eqGuard (e "a") (e "b"))]
      conditionalNew=program [assign "o" (VEIf neGuard (e "b") (e "a"))]
      historicalOnly=filter (any ((=="historical-conditional").fst).acOperations)
      recovered=get (const True) (historicalOnly (cs "F11" conditionalOld conditionalNew))
  check "comparison complement recovers typed historical conditional" (acProgram recovered==conditionalOld && acBinaryOnly recovered)
  let changedBranch=program [assign "o" (VEIf neGuard (e "b") (w 8 1))]
      changedGuard=program [assign "o" (VEIf (op (e "a") VULT (e "b")) (e "b") (e "a"))]
  check "historical conditional rejects changed branch" (null (historicalOnly (cs "F11" conditionalOld changedBranch)))
  check "historical conditional rejects noncomplement guard" (null (historicalOnly (cs "F11" conditionalOld changedGuard)))
  let mixedWidth=program [assign "o" (VEIf (op (e "a") VNE (w 7 3)) (e "b") (e "a"))]
  check "historical conditional rejects mixed-width comparison" (null (historicalOnly (cs "F11" conditionalOld mixedWidth)))
  let manyNegations n x=iterate (VEUnOp defaultVId VNot) x!!n
      giant=program [assign "o" (VEIf (manyNegations 66 neGuard) (e "b") (e "a"))]
  check "historical conditional guard analysis is bounded" (null (historicalOnly (cs "F11" conditionalOld giant)))
  let group label defs=VMComment [label] (VMGroup False [defs])
      first rhs=group "current first wrapper" [assign "x" rhs,assign "xr" (w 1 1)]
      second=group "current second wrapper" [assign "y" (e "x"),assign "yr" (w 1 1)]
      fresh1=group "new one" [assign "n1" (e "a")]
      fresh2=group "new two" [assign "n2" (e "b")]
      decls=[wire 8 "x",wire 1 "xr",wire 8 "y",wire 1 "yr"]
      newDecls=decls++[wire 8 "n1",wire 8 "n2",reg 8 "q"]
      state=VMStmt False (Valways (VAt (VEEposedge (e "c")) (VAssignA (VLId (v "q")) (e "a"))))
      priorGroup=program (decls++[first (e "a"),second,assign "o" (e "y")])
      currentGroup=program (newDecls++[second,fresh1,first (e "b"),fresh2,assign "o" (op (e "y") VXor (e "q")),state])
      groupOnly=filter (any ((=="assignment-group-order").fst).acOperations)
      groupCandidates a b=groupOnly (cs "F04" a b)
      orderedGroup=get (const True) (groupCandidates priorGroup currentGroup)
      desired=program (newDecls++[first (e "b"),fresh1,second,fresh2,assign "o" (op (e "y") VXor (e "q")),state])
  check "group order preserves changed logic new state wrappers and unmatched slots" (acProgram orderedGroup==desired && not (acBinaryOnly orderedGroup))
  let barrierFixture barrier a b=program (decls++[wire 8 "hidden",a,barrier,b,assign "o" (e "y")])
      across barrier=groupCandidates (barrierFixture barrier (first (e "a")) second)
                                     (barrierFixture barrier second (first (e "b")))
  check "group ordering respects instance barrier" (null (across (VMInst (v "Child") (v "opaque") [] [])))
  check "group ordering respects effect barrier" (null (across (VMStmt False (Vinitial (VTask (v "$display") [VEString "effect"])))))
  check "group ordering respects declaration barrier" (null (across (reg 8 "extra")))
  check "group ordering respects translate-off barrier" (null (across (VMGroup True [[assign "hidden" (e "a")]])))
  check "group ordering respects directive comment barrier" (null (across (VMComment ["synthesis keep"] (assign "hidden" (e "a")))))
  let scoped a b=program (decls++[VMGroup False [[a],[b]],assign "o" (e "y")])
  check "group ordering does not cross sibling container scopes" (null (groupCandidates (scoped (first (e "a")) second) (scoped second (first (e "b")))))
  let duplicateOld=program (decls++[first (e "a"),first (e "a"),second,assign "o" (e "y")])
  check "group ordering refuses ambiguous duplicate populated IDs" (null (groupCandidates duplicateOld currentGroup))
  let late n = VMStmt False (Valways (VAt
          (VEEOr (VEE (e "c")) (VEEOr (VEE (e "a")) (VEE (e "b"))))
          (Vcase (e "c") [VCaseArm [w 1 0] (VAssign (VLId (v n)) (e "a")),
                          VCaseArm [w 1 1] (VAssign (VLId (v n)) (e "b"))] False False)))
      oldLate=program [reg 8 "prior",late "prior",assign "o" (e "prior")]
      newLate=program [reg 8 "current",late "current",assign "o" (e "current")]
      namedLate=get ((=="F02").acFeature) (cs "F02" oldLate newLate)
  check "late process identity restores historical destination" (body (acProgram namedLate)==body oldLate)
  check "late process carries explicit strict relation" (acStateMap namedLate==[("prior","current")] && not (acBinaryOnly namedLate))
  check "late process matching rejects different widths" (null (cs "F02" oldLate (program [reg 7 "current",late "current",assign "o" (e "current")])))
  check "late process matching rejects occupied historical identity" (null (cs "F02" oldLate (program (reg 8 "prior":body newLate))))
  check "late process matching rejects duplicate historical bodies" (null (cs "F02" (program (reg 8 "other":late "other":body oldLate)) newLate))
  check "late process matching rejects duplicate current bodies" (null (cs "F02" oldLate (program (reg 8 "other":late "other":body newLate))))
  let changedLate=VMStmt False (Valways (VAt (VEE (e "c"))
        (Vcase (e "c") [VCaseArm [w 1 0] (VAssign (VLId (v "current")) (e "a")),
                        VDefault (VAssign (VLId (v "current")) (e "b"))] False False)))
  check "late process matching preserves event and default absence" (null (cs "F02" oldLate (program [reg 8 "current",changedLate,assign "o" (e "current")])))
  check "late process matching excludes directive wrappers" (null (cs "F02" oldLate (program [reg 8 "current",VMComment ["synthesis keep"] (late "current"),assign "o" (e "current")])))
  check "late process matching excludes translate-off scope" (null (cs "F02" oldLate (program [reg 8 "current",VMGroup True [[late "current"]],assign "o" (e "current")])))
  check "late process matching excludes multiple writers" (null (cs "F02" oldLate (program (assign "current" (e "a"):body newLate))))
  let groupState width state operand permission=VMRegGroup (v state) "RegUN" []
        (VMGroup False [[reg width state],[wire width operand,wire 1 permission]])
      stage width state operand permission rhs=
        [groupState width state operand permission,assign operand rhs,assign permission (w 1 1),
         VMStmt False (Valways (VAt (VEEposedge (e "c"))
           (Vif (e permission) (VAssignA (VLId (v state)) (e operand)))))]
      roleOld=program (stage 8 "retained" "operand_before" "permission_before" (e "a")++[assign "o" (e "retained")])
      roleNew=program (stage 8 "retained" "operand_before" "permission_before" (e "a")++
                       stage 8 "inserted" "operand_after" "permission_after" (e "retained")++[assign "o" (e "inserted")])
      role=get (const True) (cs "F15" roleOld roleNew)
      restore names (VProgram [m] ds comments)=VProgram [m {vm_body=mapRenameableVIds rename (vm_body m)}] ds comments
        where rename ident@(VId n src info)=maybe ident (\newName->VId newName src info) (lookup n names)
      mismatched=program (stage 8 "retained" "operand_before" "permission_before" (e "a")++
                          stage 7 "inserted" "operand_after" "permission_after" (e "retained")++[assign "o" (e "inserted")])
      collision=program (wire 8 "operand_after":body roleNew)
  check "state role matching uses typed companion groups and strict relation" (length (acStateMap role)==6 && not (acBinaryOnly role))
  check "state role matching preserves whole new program under inverse identity" (restore (acStateMap role) (acProgram role)==roleNew)
  check "state role matching preserves existing consumer spelling" (assign "o" (e "retained") `elem` body (acProgram role))
  check "state role matching rejects incompatible state widths" (null (cs "F15" roleOld mismatched))
  check "state role matching rejects companion declaration collision" (null (cs "F15" roleOld collision))
  check "state role matching respects proposal budget" (null (generateASTCandidates ["F15"] 0 roleOld roleNew) && length (generateASTCandidates ["F15"] 1 roleOld roleNew)==1)
  let repValue=op (e "a") VSub (e "b")
      repDecls=[wire 8 "historical",wire 8 "current_root",wire 8 "fanout"]
      repOld=program (repDecls++[assign "historical" (op (e "a") VAdd (e "b")),assign "current_root" (e "historical"),assign "fanout" (e "historical"),assign "o" (e "fanout")])
      repNew=program (repDecls++[assign "historical" (VEIf (e "c") (e "current_root") (e "current_root")),assign "current_root" repValue,assign "fanout" (e "current_root"),assign "o" (e "fanout")])
      repOnly=filter (any ((=="historical-alias-representative").fst).acOperations)
      repCandidates current=repOnly (cs "F08" repOld current)
      representative=get (const True) (repCandidates repNew)
      repChange target rhs (VProgram [m] ds comments)=VProgram [m {vm_body=[case x of VMAssign (VLId ident) _ | ident==v target ->assign target rhs;_->x |x<-vm_body m]}] ds comments
      repWide=program ([wire 16 "historical",wire 8 "current_root",wire 8 "fanout"]++drop 3 (body repNew))
      repCycle=repChange "current_root" (op (e "historical") VAdd (e "a")) repNew
      repIndirect=program (repDecls++[wire 8 "backedge",assign "backedge" (e "historical"),assign "historical" (e "a"),assign "current_root" (VEIf (e "c") (e "backedge") (e "backedge")),assign "fanout" (e "current_root"),assign "o" (e "fanout")])
      repDifferent=repChange "historical" (op (e "a") VAdd (e "a")) repNew
      repEffect=repChange "current_root" (VEIf (VEFctCall (v "$random") []) (e "a") (e "a")) (repChange "historical" (e "a") repNew)
      repWrap wrap (VProgram [m] ds comments)=VProgram [m {vm_body=[case x of VMAssign (VLId ident) _ | ident==v "historical" ->wrap x;_->x |x<-vm_body m]}] ds comments
  check "alias representative transfers current changed function rather than old function" (assign "historical" repValue `elem` body (acProgram representative) && not (acBinaryOnly representative))
  check "alias representative retains every declaration and destination" (all (`elem` body (acProgram representative)) repDecls && length (body (acProgram representative))==length (body repNew))
  check "alias representative rejects width mismatch" (null (repCandidates repWide))
  check "alias representative rejects existing cycle" (null (repCandidates repCycle))
  check "alias representative rejects transitive cycle introduced by extraction" (null (repCandidates repIndirect))
  check "alias representative requires equal current values" (null (repCandidates repDifferent))
  check "alias representative rejects effectful equal-arm guard" (null (repCandidates repEffect))
  check "alias representative respects directive scope" (null (repCandidates (repWrap (VMComment ["synthesis keep"]) repNew)))
  check "alias representative respects translate-off scope" (null (repCandidates (repWrap (\x->VMGroup True [[x]]) repNew)))
  check "alias representative rejects multiple drivers" (null (repCandidates (program (assign "current_root" (e "a"):body repNew))))
  check "alias representative obeys candidate bound" (null (repOnly (generateASTCandidates ["F08"] 0 repOld repNew)))
  let grouped width names=VMDecl(VVDecl VDWire (r width) [VVar(v n)|n<-names])
      partitionOnly=filter(any ((=="historical-declaration-partitions").fst).acOperations)
      partitions before after=partitionOnly(cs "F04" before after)
      declOld=program[grouped 8 ["x","y"],grouped 8 ["z"],assign "o" (e "x")]
      declNew=program[grouped 8 ["z","fresh","y","x"],assign "o" (e "b")]
      declProposal=get (const True)(partitions declOld declNew)
      declExpected=[grouped 8 ["x","y"],grouped 8 ["z"],grouped 8 ["fresh"],assign "o" (e "b")]
  check "declarations restore old member partitions and order" (body(acProgram declProposal)==declExpected)
  check "declarations keep current changed function" (assign "o" (e "b") `elem` items declProposal)
  check "declarations preserve newly added variables separately" (grouped 8 ["fresh"] `elem` items declProposal)
  let splitCurrent=program[grouped 8 ["y"],grouped 8 ["x"],grouped 8 ["z"],assign "o" (e "b")]
      joined=get (const True)(cs "F04" declOld splitCurrent)
  check "declarations recover split old group inside current segment" (body(acProgram joined)==[grouped 8 ["x","y"],grouped 8 ["z"],assign "o" (e "b")])
  let wideCurrent=program[grouped 8 ["y","fresh"],grouped 9 ["x"],grouped 8 ["z"],assign "o" (e "b")]
      widthKept=get(const True)(partitions declOld wideCurrent)
  check "declarations preserve current width growth" (grouped 9 ["x"] `elem` items widthKept && grouped 8 ["x","y"] `notElem` items widthKept)
  let freshOnly=program[grouped 8 ["novel1","novel2"],assign "o"(e "a")]
  check "declarations do not regroup without historical identities" (null(partitions declOld freshOnly))
  let deleted=program[grouped 8 ["z","x"],assign "o" (e "b")]
      deletedKept=get(const True)(partitions declOld deleted)
  check "declarations never resurrect removed old members" (all (not.isInfixOf "VId \"y\"".show)(items deletedKept))
  let wrapped wrap m=program[wrap(VMGroup False[body m])]
  check "declaration directives remain barriers" (null(partitions declOld (wrapped(VMComment ["synthesis keep"])declNew)))
  check "declaration translate-off remains barrier" (null(partitions declOld (program[VMGroup True[body declNew]])))
  let blocker=assign "fresh"(e "a")
      separated=program[grouped 8["y"],blocker,grouped 8["x"],assign "o"(e "b")]
  check "declaration grouping cannot cross executable item" (null(partitions declOld separated))
  let initialized=VMDecl(VVDWire(r 8)(VVar(v "fresh"))(e "a"))
  check "initialized declarations are grouping barriers" (null(partitions declOld(program[grouped 8["y"],initialized,grouped 8["x"],assign "o"(e "b")])))
  let regWrapper n=VMRegGroup(v n)"RegUN"[](VMGroup False[[reg 8 n],[wire 8(n++"_data"),wire 1(n++"_enable")]])
      oldRegs=program[regWrapper "first",regWrapper "second",assign "o"(e "first")]
      newRegs=program[regWrapper "second",regWrapper "new_stage",regWrapper "first",assign "o"(e "new_stage")]
      placed=get(any((=="historical-register-placement").fst).acOperations)(cs "F04" oldRegs newRegs)
  check "register declarations recover old group order with added state" (body(acProgram placed)==[regWrapper "new_stage",regWrapper "first",regWrapper "second",assign "o"(e "new_stage")])
  check "register declarations keep current state scaffolding" (all (`elem` items placed)(concatMap flatten[[regWrapper "first"],[regWrapper "second"],[regWrapper "new_stage"]]))
  let interrupted=program[regWrapper "second",assign "o"(e "a"),regWrapper "first"]
  check "register placement cannot cross logic boundary" (null(filter(any((=="historical-register-placement").fst).acOperations)(cs "F04" oldRegs interrupted)))
  check "declaration feature obeys candidate bound" (null(generateASTCandidates["F04"]0 declOld declNew))
  let infoGroup xs=VMComment ["current declaration group"](VMGroup False[xs])
      infoOld=program[infoGroup[grouped 8["x","y"]],assign "o"(e "x")]
      infoNew=program[infoGroup[grouped 8["new_member","y","x"]],assign "o"(e "b")]
      regionOnly=filter(any((=="historical-declaration-regions").fst).acOperations)
      infoRecovered=get(const True)(regionOnly(cs "F04" infoOld infoNew))
      expectedInfo=program[VMGroup False[[grouped 8["new_member"]]],infoGroup[grouped 8["x","y"]],assign "o"(e "b")]
  check "novel declarations placed separately outside existing historical group" (acProgram infoRecovered==expectedInfo)
  let regionBarrier=program[infoGroup[grouped 8["new_member","y","x"]],VMComment["synthesis keep"](grouped 8["protected"]),grouped 8["other_new"],assign "o"(e "b")]
      barrierRecovered=get(const True)(regionOnly(cs "F04" infoOld regionBarrier))
      barrierBody=body(acProgram barrierRecovered)
  check "declaration region never crosses directive boundary" (drop 2 barrierBody==[VMComment["synthesis keep"](grouped 8["protected"]),grouped 8["other_new"],assign "o"(e "b")])
  let directInfoNew=program[VMComment["current declaration group"](grouped 8["new_member","y","x"]),assign "o"(e "b")]
      directInfoRecovered=get(const True)(regionOnly(cs "F04" infoOld directInfoNew))
  check "direct comment-wrapped declaration separates novel members" (grouped 8["new_member"] `elem` items directInfoRecovered && grouped 8["x","y"] `elem` items directInfoRecovered)
  check "feature table names unique implemented codes" (map fst featureTable==["F01","F02","F03","F04","F06","F07","F08","F09","F10","F11","F15"] && all (not.null.snd) featureTable)
  let dbu=declaredBeforeUse; lex=lexicallyEmittable
      childUse=VMInst (v "Child") (v "child") [] [(v "A",Just (e "x"))]
  check "use before declaration is rejected with the offending name" (dbu (program [assign "o" (e "x"),wire 8 "x",assign "x" (e "a")])==Left "x")
  check "declaration partition accepted" (dbu (program [wire 8 "x",assign "x" (e "a"),assign "o" (e "x")])==Right ())
  check "instance connections are uses" (dbu (program [childUse,wire 8 "x",assign "x" (e "a")])==Left "x")
  check "initialized wire uses only earlier names" (dbu (program [VMDecl (VVDWire (r 8) (VVar (v "x")) (e "y")),wire 8 "y"])==Left "y")
  check "system tasks and ports need no declaration" (dbu (program [VMStmt False (Vinitial (VTask (v "$display") [VEString "effect",e "a"]))])==Right ())
  check "newline inside an identifier is rejected" (lex (program [wire 8 "x\n assign o = 8'd0;"])==Left "x\n assign o = 8'd0;")
  check "newline inside a comment element is rejected" (lex (program [VMComment ["note\nassign o = 8'd0;"] (assign "o" (e "a"))])==Left "note\nassign o = 8'd0;")
  check "register group id is printed" (lex (program [VMRegGroup (v "r\n") "Reg" [] (reg 8 "r")])==Left "r\n")
  check "header comment lines are printed" (lex (VProgram [VModule (v "M") ["a\nb"] [] []] [] [])==Left "a\nb")
  check "annotation ids are not printed" (lex (program [assign "o" (VEOp (mkVId "") (e "a") VAdd (VEWConst (mkVId "x\n") 8 10 1))])==Right ())
  check "system functions and hierarchical names are identifiers" (lex (program [assign "o" (VEFctCall (v "$signed") [e "u.q"])])==Right ())
  putStrLn "PASS native AST transforms: 139 directed checks"
