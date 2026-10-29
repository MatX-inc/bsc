module Main where
import Control.Exception(SomeException,try)
import Control.Monad(unless)
import qualified Data.ByteString as B
import Data.List(isInfixOf)
import System.Directory(createDirectory)
import System.Environment(getArgs)
import System.FilePath((</>))
import BinData(encode)
import ECO.VerilogInfo
import ECOASTMetric
import ECOASTTranslatedMetric
import ECOJSON(EvidenceJSON(..),parseEvidenceJSON)
import ECOASTCost(changedLines)
import Verilog
import PPrint(pp80)
v=mkVId
e=VEVar . v
assign n x=VMAssign(VLId(v n))x
reg names=VMDecl(VVDecl VDReg Nothing(map(VVar . v)names))
program body=VProgram[VModule(v"M")[] [([VAInput(v"a")Nothing,VAOutput(v"o")Nothing],[])] body][][]
project p=either error fst(projectTranslatedProgram p)
summary p=either error snd(projectTranslatedProgram p)
check s b=unless b(fail s)>>putStrLn("PASS: "++s)
reject s action=do
 x<-try(action>>pure())::IO(Either SomeException())
 check s(either(const True)(const False)x)
task=VTask(v"$display")[VEString"old debug"]
assertion=VAssert(VEEposedge(e"clk"))[VEString"property_name",VEString"%b",e"a",e"o"]
initial=Vinitial(VSeq[VAssign(VLId(v"q"))(VEConst 1)])
main=do
 args<-getArgs
 case args of
  ["pure",out]->do
   createDirectory out
   let x=assign"x"(e"a");y=assign"y"(e"a");z=assign"z"(e"a")
       base=program[x,y]
       off s=VMStmt True s
       offGroup xs=VMGroup True[xs]
       norm=normalizeProgram.project
   check"ordinary active AST retained"(show(project base)==show base)
   check"off statement removed"(norm(program[x,off task,y])==norm base)
   check"off group removes declarations and statements"(norm(program[x,offGroup[reg["private"],VMStmt False task],y])==norm base)
   check"off initial and its guard removed"(not("BSV_NO_INITIAL_BLOCKS"`isInfixOf`pp80(project(program[off initial]))))
   check"off initial accounting"(removedInitialBlocks(summary(program[off initial]))==1)
   check"unflagged initial retained including macro guard"("BSV_NO_INITIAL_BLOCKS"`isInfixOf`pp80(project(program[VMStmt False initial])))
   check"off assertion accounting"(removedAssertions(summary(program[off assertion]))==1)
   check"unflagged assertion retained"(show(project(program[VMStmt False assertion]))==show(program[VMStmt False assertion]))
   check"off task accounting"(removedTaskCalls(summary(program[off task]))==1)
   check"nested native off/on enables later outer body"(norm(program[offGroup[x,offGroup[y],z]])==norm(program[z]))
   check"nested statement off/on enables later outer body"(norm(program[offGroup[x,off task,z]])==norm(program[z]))
   check"nested wrapper accounting"(nestedOffWrappers(summary(program[offGroup[x,offGroup[y],z]]))==1)
   check"unquoted assertion switch rejected"(case projectTranslatedProgram(program[off(VAssert(VEEposedge(e"clk"))[VEString"name",VEString"// translate_on"])])of Left _->True;_->False)
   check"outer off/on leaves following item enabled"(norm(program[offGroup[x],z])==norm(program[z]))
   check"transparent group carries actual toggle state"(norm(program[offGroup[x,VMGroup False[[off task],[y]],z]])==norm(program[y,z]))
   check"active declaration grouping retained"(show(project(program[reg["x","y"]]))==show(program[reg["x","y"]]))
   check"declaration partitions remain material"(norm(program[reg["x","y"]])/=norm(program[reg["x"],reg["y"]]))
   check"active helper used only by excluded debug still counts"("assign helper"`isInfixOf`norm(program[assign"helper"(e"a"),off(VTask(v"$display")[e"helper"])]))
   check"active comment before off child remains raw"("live comment"`isInfixOf`pp80(project(program[VMComment["live comment"](off task)])))
   check"comment emitted inside off region disappears"(not("hidden comment"`isInfixOf`pp80(project(program[offGroup[VMComment["hidden comment"](off task)]]))))
   check"register comment before off child remains raw"("register q"`isInfixOf`pp80(project(program[VMRegGroup(v"q")"Reg"[](off initial)])))
   check"arbitrary macro expression preserved"("`MY_CONFIGURATION"`isInfixOf`norm(program[assign"o"(VEMacro"MY_CONFIGURATION")]))
   check"quoted string is not treated as directive"(case projectTranslatedProgram(program[assign"o"(VEString"translate_off")])of Right _->True;_->False)
   check"comment textual switch rejected"(case projectTranslatedProgram(program[VMComment["synopsys translate_off"]x])of Left _->True;_->False)
   check"hidden comment textual switch rejected"(case projectTranslatedProgram(program[offGroup[VMComment["synthesis translate_on"]x]])of Left _->True;_->False)
   let VProgram[m]d c=base
   check"module comment switch rejected"(case projectTranslatedProgram(VProgram[m{vm_comments=["translate_off"]}]d c)of Left _->True;_->False)
   check"port comment switch rejected"(case projectTranslatedProgram(VProgram[m{vm_ports=[([], ["translate_on"])]}]d c)of Left _->True;_->False)
   check"program comment switch rejected"(case projectTranslatedProgram(VProgram[m]d["translate_off"])of Left _->True;_->False)
   check"projection is idempotent"(show(project(project(program[x,offGroup[y],z])))==show(project(program[x,offGroup[y],z])))
  ["captures",oldPath,newPath,out]->do
   createDirectory out
   r<-measureTranslatedAST(out</>"ordinary")(Snapshot oldPath)(Snapshot newPath)
   check"captured debug change removed from translated normalized diff"(changedLines(translatedNormalized r)==0)
   check"whole-file debug change remains recorded"(changedLines(metricNormalized(translatedWhole r))>0)
   check"translated report embeds the complete typed whole-file report"
     (case parseEvidenceJSON(renderTranslatedMetricReport r) of
        Right(JObject fields)->lookup "whole_file" fields==Just(metricReportJSON(translatedWhole r))
        _->False)
   check"captured simulation initialization excluded"(removedInitialBlocks(translatedOldProjection r)>0)
   check"captured debug tasks excluded"(removedTaskCalls(translatedOldProjection r)>0)
   new<-readVerilogInfo newPath
   let candidate=viProgram new
       ast=out</>"candidate.ast";rtl=out</>"candidate.v"
   B.writeFile ast(B.pack(encode candidate));writeFile rtl(renderVerilogText new candidate)
   c<-measureTranslatedAST(out</>"candidate")(Snapshot oldPath)(ContextAST newPath ast rtl)
   check"candidate metrics equal authenticated ordinary"(translatedRaw c==translatedRaw r&&translatedNormalized c==translatedNormalized r)
   reject"fresh output required"(measureTranslatedAST(out</>"candidate")(Snapshot oldPath)(Snapshot newPath))
   appendFile rtl"// changed bytes\n"
   reject"unbound actual candidate RTL rejected"(measureTranslatedAST(out</>"bad-bytes")(Snapshot oldPath)(ContextAST newPath ast rtl))
   check"metric claims no equivalence proof"("\"equivalence_proof_performed\":false"`isInfixOf`renderTranslatedMetricReport r)
  _->fail"ASTTranslatedMetric.hs pure OUT | captures OLD NEW OUT"
