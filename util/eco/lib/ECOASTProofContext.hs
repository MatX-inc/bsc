-- Interpret native child interface descriptors for the independent AST checker.
-- These describe actual port directions/types, never behavioral assumptions.
module ECOASTProofContext (childPinsFromInstances) where

import ASyntax (AVInst(..),AType(..),AExpr(..),getSpecialOutputs)
import Id (getIdString)
import VModInfo
import ECOASTProof (ChildDirection(..),ChildPin(..))
import Data.List (nub)

childPinsFromInstances :: [AVInst] -> [ChildPin]
childPinsFromInstances = nub . concatMap instancePins
  where
    width (ATBit w) | w>0 = Just w
    width _ = Nothing
    instancePins inst =
      let instName=getIdString (avi_vname inst)
          vmi=avi_vmi inst
          pin dir port ty=ChildPin instName (getVPortString port) dir (width ty)
          named dir n w=ChildPin instName (getVNameString n) dir w
          args=[pin ChildInput port (ae_type value) |
                  (Port port _ _,value)<-zip (vArgs vmi) (avi_iargs inst)]
          inouts=[named ChildInout n Nothing | InoutArg n _ _<-vArgs vmi]
          clocks=[named ChildInput n (Just 1)|n<-getInputClockPorts (vClk vmi)]
          resets=[named ChildInput n (Just 1)|n<-getInputResetPorts (vRst vmi)]
          methods=concat [methodPins pin f ts | (f,ts)<-zip (vFields vmi) (avi_meth_types inst)]
          specials=[pin (if VPinout `elem` props then ChildInout else ChildOutput) port ty |
                     (_,ty,port@(_,props))<-getSpecialOutputs inst]
      in args++inouts++clocks++resets++methods++specials
    -- Multi-copy methods may suffix formal names during final instantiation;
    -- omit them rather than assert an unverified binding to a single copy.
    methodPins pin f@(Method {}) (inputs,en,outputs)
      | vf_mult f<=1 && length (concat (vf_inputs f))==length (concat inputs) &&
        length (vf_outputs f)==length outputs =
          [pin ChildInput p t | (p,t)<-zip (concat (vf_inputs f)) (concat inputs)]++
          [pin ChildOutput p t | (p,t)<-zip (vf_outputs f) outputs]++
          case (vf_enable f,en) of (Just p,Just t)->[pin ChildInput p t];_->[]
    methodPins _ _ _ = []
