module CoreM where
import Warmup ()
import qualified Data.Map as M
coreVal :: M.Map Int Int
coreVal = M.fromList [(1,2)]
