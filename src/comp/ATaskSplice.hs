module ATaskSplice(aTaskSplice) where
import ASyntax
import ASyntaxUtil
import Id
import Data.Ord(Down(..))
import qualified Data.Map as M
import ErrorUtil(internalError)

-- splices the values assigned into ATaskAction by walking the ADefs

type SpliceMap = M.Map Integer (Id,AType)

aTaskSplice :: APackage -> APackage
aTaskSplice apkg = mapAActions (spliceAction spliceMap) apkg
  where spliceMap = M.map snd $
                    M.fromListWith pickBest
                      [ (n, ((idQuality (Just i), Down ord), (i, t)))
                      | (ord, ADef i t (ATaskValue { ae_cookie = n }) _)
                          <- zip [0 :: Int ..] defs ]
        pickBest new@(kNew, _) old@(kOld, _)
          | kNew > kOld = new
          | otherwise   = old
        defs = (apkg_local_defs apkg) ++
               (concatMap av_ret_def (apkg_interface apkg))
        av_ret_def act@(AIActionValue {}) = [aif_value act]
        av_ret_def _                      = []

spliceAction :: SpliceMap -> AAction -> AAction
spliceAction spliceMap a@(ATaskAction { ataskact_temp = Nothing }) =
  case (M.lookup (ataskact_cookie a) spliceMap) of
    (Just (temp,temp_ty)) ->
      if temp_ty == (ataskact_value_type a)
      then a { ataskact_temp = (Just temp) }
      else internalError ("ATaskSplice.spliceAction: temp. type mismatch: " ++
                          (show temp_ty) ++
                          " vs. expected " ++
                          (show (ataskact_value_type a)))
    Nothing -> a
spliceAction _ a@(ATaskAction { ataskact_temp = (Just _) }) =
    internalError ("ATaskSplice.spliceAction: " ++ (show a))
spliceAction _ a = a

