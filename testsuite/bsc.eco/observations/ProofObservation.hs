{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Main where
import Control.Monad.State
import Control.Monad (unless)
import ProofObligation

instance HasProve (ProofObligation Int) (StateT [Int] IO) where
  prove (ProveEq value _) = do
    modify (++[value])
    return ([Proven,Disproven,Inconclusive] !! (value `mod` 3))
  prove (ProveNotEq _ _) = fail "unexpected obligation"

main = do
  let quiet name=MsgFn name (\_ messages->messages)
      requests :: [(ProofObligation Int,MsgFn)]
      requests=[(ProveEq 3 0,quiet "z"),(ProveEq 0 0,quiet "a"),
                (ProveEq 1 0,quiet "a"),(ProveEq 2 0,quiet "a")]
      unused=error "quiet observation should not force the diagnostic handle"
  ((observedOK,results),observedOrder)<-runStateT (checkProofsObserved unused requests) ([] :: [Int])
  (ordinaryOK,ordinaryOrder)<-runStateT (checkProofs unused requests) ([] :: [Int])
  unless (ordinaryOK && observedOK && ordinaryOrder==[0,1,2,3] && ordinaryOrder==observedOrder)
    (fail "observation changed ordinary grouping, attempts, order or success")
  unless ([r|(_,_,r)<-results]==[Proven,Disproven,Inconclusive,Proven])
    (fail "success was conflated with proven obligations")
  putStrLn "PASS: ordinary and observed proof attempts agree, all three outcomes retained"
