module ECOASTPreservation(directiveFree,transparentFlat,mapTransparent) where
import Verilog
import qualified Data.Generics as G
import Data.List(isInfixOf)
import Data.Char(toLower)
-- Informational comments may be preserved, but directives are not proof-free
-- layout annotations. Include every native AST comment field, not just wrappers.
directiveFree(VProgram ms _ cs)=all safe(cs++concatMap comments ms) where
 safe c=not(any(`isInfixOf`map toLower c)markers)
 markers=["full_case","parallel_case","synopsys","synthesis","pragma","translate_","attribute","(*","*)","`",
          "synplify","altera","xilinx","quartus","cadence","vivado","verilator",
          "verible","lint","coverage","dont_touch","dont touch","keep_hierarchy","mark_debug","async_reg"]
 comments m=vm_comments m++concatMap snd(vm_ports m)++
  concat[ss|VMComment ss _<-G.listify(const True)(vm_body m)]++
  concat[ss|VMRegGroup _ _ ss _<-G.listify(const True)(vm_body m)]
-- Translate-off and opaque statements remain indivisible semantic barriers.
transparentFlat=concatMap f where
 f(VMGroup False xs)=transparentFlat(concat xs)
 f(VMComment _ x)=f x
 f(VMRegGroup _ _ _ x)=f x
 f x=[x]
mapTransparent f=map go where
 go(VMGroup False xs)=VMGroup False(map(mapTransparent f)xs)
 go(VMComment cs x)=VMComment cs(one(mapTransparent f[x]))
 go(VMRegGroup n cls cs x)=VMRegGroup n cls cs(one(mapTransparent f[x]))
 go x@(VMGroup True _)=x
 go x@(VMStmt True _)=x
 go x=f x
 one[x]=x
 one xs=VMGroup False[xs]
