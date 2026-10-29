module Main where
import Control.Exception (SomeException,try)
import Control.Monad (unless)
import ECO.VerilogInfo
import System.Environment (getArgs)
import System.FilePath ((</>))
import Verilog
check name okay = do
  unless okay (fail name)
  putStrLn ("PASS: "++name)
reject name action = do
  result<-try(action>>return()) :: IO(Either SomeException ())
  check name (either (const True) (const False) result)
marker (VProgram ms ds cs) = VProgram ms ds (cs++["ECO_CANDIDATE_MARKER"])
main = do
  [identityPath,candidatePath,ordinaryPath,macroPath,macroCandidatePath,out]<-getArgs
  identity<-readVerilogInfo identityPath
  candidate<-readVerilogInfo candidatePath
  ordinary<-readVerilogInfo ordinaryPath
  macro<-readVerilogInfo macroPath
  macroCandidate<-readVerilogInfo macroCandidatePath
  identityExact<-verilogRenderingIsExact identity
  candidateExact<-verilogRenderingIsExact candidate
  ordinaryExact<-verilogRenderingIsExact ordinary
  check "identity filters bind exact ordinary AST bytes" (identityExact&&candidateExact)
  check "ordinary-changing filter is explicitly nonidentity" (not ordinaryExact)
  _<-verifyVerilogInfo ordinary (out</>"ordinary-control")
  emitVerilog identity (out</>"identity-candidate.v") (marker(viProgram identity))
  check "candidate with observed byte-preserving filter is emitted" True
  reject "filter must preserve this candidate's bytes"
    (emitVerilog candidate (out</>"rejected-candidate.v") (marker(viProgram candidate)))
  reject "nonidentity ordinary filter cannot authorize AST transform"
    (emitVerilog ordinary (out</>"rejected-ordinary.v") (marker(viProgram ordinary)))
  macroExact<-verilogRenderingIsExact macro
  check "upstream parameter policy binds complete ordinary bytes" macroExact
  _<-verifyVerilogInfo macro (out</>"macro-control")
  emitVerilog macro (out</>"macro-candidate.v") (marker(viProgram macro))
  check "upstream parameter candidate passes actual filter byte control" True
  reject "upstream parameter policy cannot hide another filter change"
    (emitVerilog macroCandidate (out</>"rejected-macro-candidate.v") (marker(viProgram macroCandidate)))
