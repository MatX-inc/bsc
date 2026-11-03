{-# LANGUAGE LambdaCase, MagicHash, DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}
-- | The schedule verifier's model of one bsc module (the Design).
module Lean.Sched.Types where

import Control.Exception(evaluate)
import Control.Monad(foldM, forM, forM_, when)
import Control.Monad.State.Strict(State, evalState, gets, modify', runState)
import qualified Data.ByteString as BS
import Data.Char(isAsciiLower, isAsciiUpper, isDigit, toUpper)
import Data.Containers.ListUtils(nubOrd)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import qualified Data.Foldable as F
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import GHC.Exts(isTrue#, reallyUnsafePtrEquality#)
import Data.List(intercalate, isPrefixOf, mapAccumL, partition, sortOn, tails, transpose)
import qualified Data.Map.Lazy as ML
import qualified Data.Map.Strict as M
import Data.Maybe(catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import qualified Data.Sequence as Sq
import qualified Data.Set as S
import System.Console.GetOpt(ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt, usageInfo)
import System.Environment(getArgs)
import System.Exit(exitFailure, exitSuccess)
import System.FilePath(takeFileName)
import System.IO.Unsafe(unsafePerformIO)
import System.IO(IOMode(..), hPutStr, hPutStrLn, hSetEncoding, stderr,
                 stdout, utf8, withFile)

import Control.DeepSeq(NFData)
import GHC.Generics(Generic)
import ABin(ABin(..), ABinModInfo(..))
import ASchedule(AScheduleInfo(..), MethodId(..), UniqueUse(..))
import AScheduleInfo(SchedNode(..), erdbToList)
import AState(aState)
import AVerilogUtil(vNameToTask)
import ASyntax
import ASyntaxUtil(aType, aAnd, aOr, aOrs, aSubst, argInputPorts)
import AUses(Rule(..), buildUseMaps, extractCondition, rumToMethodUseMap,
             ruleName, rulePred, ruleAncestor)
import BackendNamingConventions(isRWire, isRWire0, isBypassWire)
import DisjointTest(initDisjointTestState, checkDisjointExprWithCtx)
import Error(ErrorHandle, initErrorHandle)
import Flags(Flags, unSpecTo, useDPI)
import GenABin(readABinFile)
import Id(getIdString, getIdBaseString, mkIdCanFire, mkIdWillFire, mkRdyId,
          isRdyId, getIdPosition, setIdBaseString, mk_homeless_id)
import IntegerUtil(aaaa)
import IntLit(IntLit(..))
import PPrint(ppString)
import Pragma(SchedulePragma(..))
import Prim(PrimOp(..))
import RSchedule(ratToNestedLists)
import SchedInfo(SchedInfo(..), MethodConflictInfo(..))
import VModInfo(vName, vFields, vSched, vPath, VFieldInfo(..), VPathInfo(..),
                getVNameString, mkNamedOutputs)


-- | A child submodule instance, seen only through its 'VModInfo'.
data Child = Child
    { chInst    :: String
    , chModule  :: String                    -- ^ Verilog module, e.g. @RegN@
    , chMethods :: M.Map String Meth
    , chOrder   :: [String]                  -- ^ methods, as declared
    , chConf    :: Conflicts
    , chSched   :: String                    -- ^ the annotations, for comments
    , chAlias   :: M.Map String String
      -- ^ result wire of an argument-free value method -> the first such wire
      -- reading the same Verilog output port (e.g. a FIFO's notEmpty and
      -- i_notEmpty are both EMPTY_N), so they are one variable
    , chWires   :: [(String, String)]
      -- ^ for bsc's wire primitives, (output, input) signal pairs equal
      -- within the cycle: RWire.v assigns WGET = WVAL and WHAS = WSET
    }

data MethKind = ValueMethod | ActionMethod | ActionValueMethod
    deriving (Eq, Show)

-- | A method of a child.
data Meth = Meth
    { mtName   :: String
    , mtKind   :: MethKind
    , mtCopies :: [Port]       -- ^ the copies in use
    , mtDeps   :: [String]     -- ^ action methods whose same-cycle calls it sees
    }

-- | One copy of a child method, named by its hardware signals: @r$EN_write@,
-- @r$write_1@ and @r$read@ for a single-copy method, @rf$sub_1_1@ and
-- @rf$sub_1@ for copy 1 of a multi-ported one.
data Port = Port
    { ptInst    :: String
    , ptMeth    :: String
    , ptCopy    :: Maybe Integer
    , ptKind    :: MethKind
    , ptEnable  :: Maybe String
    , ptArgs    :: [(String, AType)]
    , ptResults :: [(String, AType)]
    }

instance Eq Port where
    a == b = portKey a == portKey b

instance Ord Port where
    compare a b = compare (portKey a) (portKey b)

portKey :: Port -> (String, String, Maybe Integer)
portKey p = (ptInst p, ptMeth p, ptCopy p)

-- | @inst.meth@, or @inst.meth[k]@ for copy @k@ of a multi-ported method.
portLabel :: Port -> String
portLabel p = ptInst p ++ "." ++ ptMeth p ++ maybe "" (\k -> "[" ++ show k ++ "]") (ptCopy p)

-- | Which pairs of methods of one child may share a cycle, and how.  Like
-- bsc's scheduler ('ASchedule.getStateInfo'), only CF, SB and SBR permit
-- anything; P, ME, EXT, C and unannotated pairs all conflict.
data Conflicts = Conflicts
    { cfCF  :: S.Set (String, String)   -- ^ symmetric
    , cfSB  :: S.Set (String, String)   -- ^ @(a, b)@: a SB b
    , cfSBR :: S.Set (String, String)   -- ^ @(a, b)@: a SBR b
      -- For each method, the methods that conflict with it (built lazily,
      -- so only for the methods in use), so that a use is compared only
      -- with the uses it can conflict with:
    , cfNoFollow  :: M.Map String [String]  -- ^ those that may not follow it
    , cfNoPrecede :: M.Map String [String]  -- ^ those it may not follow
    , cfNoShare   :: M.Map String [String]  -- ^ those it may not share a rule with
    }

-- | Uses (anything with a 'Port'), indexed for 'conflicting': by instance
-- and method, and by port.  Each bucket keeps the uses in the order given.
data UseIndex a = UseIndex (M.Map (String, String) [a]) (M.Map (String, String, Maybe Integer) [a])

-- | Which conflicts 'conflicting' looks for: uses that may not follow a
-- use, that it may not follow, or that may not share a rule with it.
data ConflictKind = NoFollow | NoPrecede | NoShare

-- | A schedule entry: a rule, the body of an action method, or a value method.
data Step = Step
    { stName     :: String           -- ^ e.g. @RL_incA@, @put@, @get@
    , stMethod   :: Maybe String
      -- ^ the interface method it belongs to (a method bsc split into
      -- several rules has a step for each)
    , stKind     :: StepKind
    , stDescr    :: String           -- ^ where it comes from, for comments
    , stFire     :: String           -- ^ its WILL_FIRE definition
    , stCanFire  :: String           -- ^ its CAN_FIRE definition
    , stGuard    :: Maybe AExpr      -- ^ rule predicate
    , stCalls    :: [Call]           -- ^ child action-method calls, in order
    , stReads    :: [(Port, AExpr)]
      -- ^ value-method copies read, each with the condition bsc's use
      -- analysis says it matters under: only a hint of which disjointness
      -- facts to ask bsc's solver for
    , stReadSites :: [Port]
      -- ^ the value-method copies its guard, calls and outputs read, found
      -- in the expressions themselves (through definitions)
    , stChecks   :: [AExpr]          -- ^ conditions of bsc's run-time checks
    , stTaskReads :: [(AExpr, [AExpr], [Port])]
      -- ^ each system task's condition, its arguments, and the copies the
      -- two read
    , stSites    :: [Site]           -- ^ its reads the annotations rule out
    , stTaskSites :: [(AExpr, [AExpr], [Site])]
      -- ^ those of its system tasks, with each task's condition and
      -- arguments (every task, in the order of 'stTaskReads')
    , stTaskCalls :: [TaskCall]
      -- ^ its system-task calls, bsc's run-time checks among them, in
      -- program order, each with its index in the cycle's ('tcTag')
    , stOutputs  :: [Output]         -- ^ module outputs observed at this step
    }

-- | A read the submodule's annotations rule out where a step makes it: the
-- value-method copy read, and the use of the same submodule it may not come
-- after.  That use is a call of an action copy by an earlier step (in some
-- checked order), or by the step itself when the two may not share a rule
-- ('siWithin'); or a read of another value method by an earlier step, which
-- counts when the value it read reached what that step did ('dReadFlags').
-- Such a read sees an arbitrary value when the step fires and the use was
-- made.
data Site = Site { siPort :: Port, siUse :: Port, siWithin :: Bool }

data StepKind = RuleStep | ActionMethodStep | ValueMethodStep
    deriving (Eq, Show)

-- | A system-task (or foreign-function) call: the task as Verilog names it,
-- its id and source position, its condition and its arguments.
data TaskCall = TaskCall
    { tcName  :: String
    , tcId    :: String
    , tcCond  :: AExpr
    , tcArgs  :: [AExpr]
    , tcCheck :: Bool                -- ^ one of bsc's run-time checks
    , tcTag   :: Int                 -- ^ its index among the cycle's calls
    , tcPos   :: String              -- ^ its source position, for comments
    }

-- | A call of the hardware's always block: the step whose WILL_FIRE its
-- condition starts with, and the sequential call it is ('matchTasks').
data HwTask = HwTask
    { htName :: String
    , htId   :: String
    , htStep :: Maybe String
    , htCond :: AExpr
    , htArgs :: [AExpr]
    , htTag  :: Maybe Int
    }

-- | A guarded call of a child action method.
data Call = Call
    { clPort :: Port
    , clCond :: AExpr
    , clArgs :: [AExpr]              -- ^ one per hardware argument port
    }

-- | A module output, and when the environment may rely on it.
data Output = Output
    { ouName :: String               -- ^ hardware output port, e.g. @RDY_get@
    , ouSeq  :: AExpr                -- ^ its value in the sequential semantics
    , ouMask :: [Mask]               -- ^ compared only when all of these hold
    }

data Mask = WhenOutput String        -- ^ another output (an RDY) is set
          | WhenInput String         -- ^ a module input (an EN) is set
          | WhenNotInput String      -- ^ a module input (an EN) is clear
    deriving (Eq, Show)

-- | Everything the proof needs, extracted from the @.ba@ and from 'aState'.
data Design = Design
    { dModule     :: String
    , dSource     :: FilePath
    , dVersion    :: String
    , dPackage    :: String
    , dChildren   :: M.Map String Child
    , dChildOrder :: [String]
    , dSteps      :: [Step]
    , dStepMap    :: M.Map String Step     -- ^ each step by name (the first, if repeated)
    , dMethodSteps :: M.Map String [String] -- ^ the steps of each method, in order
    , dSeqDefs    :: M.Map String ADef     -- ^ APackage definitions
    , dHwDefs     :: M.Map String ADef     -- ^ ASPackage definitions
    , dInputs     :: M.Map String AType    -- ^ module inputs (args, enables)
    , dInputOrder :: [String]
    , dOutputs    :: [(String, AType)]     -- ^ module outputs
    , dStateOuts  :: M.Map String (Port, Int)
      -- ^ child output wire, e.g. @a$read@: its port and result index
    , dUseCopy    :: M.Map (String, String) (M.Map AExpr Integer)
    , dDisjoint   :: [(String, String)]    -- ^ CAN_FIRE-disjoint, per bsc
    , dDisjointSet :: S.Set (String, String) -- ^ the same, for membership tests
    , dBlocks     :: S.Set (String, String) -- ^ @(r, b)@: urgency lets b block r
    , dUserME     :: [(String, String)]    -- ^ @mutually_exclusive@ pairs
    , dExportedC  :: [(String, String)]    -- ^ own methods exported C
    , dExportedCBy :: M.Map String [String]
      -- ^ each method's partners in 'dExportedC', in the order of its pairs
    , dExportedSB :: [(String, String, String)] -- ^ own methods exported SB/SBR
    , dEnables    :: M.Map String String   -- ^ method -> its enable port (an input)
    , dReady      :: M.Map String String   -- ^ method -> its ready port (an output)
    , dReadyDef   :: M.Map String String   -- ^ method -> the definition of its ready signal
    , dOrders     :: [Order]
    , dExportedCF :: [(String, String)]     -- ^ exported CF pairs of methods with steps
    , dUseDefs    :: [ADef]                 -- ^ the use-condition definitions recomputed
    , dUnrealized :: [(String, String)]
      -- ^ exported CF pairs no order of the steps can swap (left unchecked)
    , dHavocPorts :: S.Set Port
      -- ^ the value-method copies some step reads where the annotations rule
      -- it out ('stSites')
    , dReadFlags  :: [Port]
      -- ^ value-method copies a later read of another value method may not
      -- follow: whether a step's read of each mattered this cycle is recorded
    , dRelied     :: [String]
      -- ^ the CAN_FIRE signals of the steps whose disjointness facts left
      -- reads out of 'stSites' ('havocSites') because of their sequential
      -- firing ('ViaSequential'): each must be one definition in both
      -- semantics
    , dUnspec     :: String                -- ^ @-unspecified-to@
    , dHwTasks    :: [HwTask]
      -- ^ the system-task calls of the hardware's always block, in the
      -- order bsc's Verilog back end prints them
    , dTaskTags   :: Int                   -- ^ how many sequential calls there are
    , dWarnings   :: [String]
    }

-- | A logical execution order: the schedule's own, or the same with two of
-- the module's methods, which it exports as conflict-free, swapped.  A parent
-- may call such methods in either order, so both must be right.
data Order = Order
    { orName  :: String     -- ^ @""@ for the schedule's own order
    , orPair  :: (String, String)
    , orSteps :: [String]   -- ^ step names
    , orFrom  :: Int        -- ^ the first position that differs from the schedule's
    , orWindow :: [String]
      -- ^ its steps from 'orFrom' to a position from which on it is the
      -- schedule's again
    , orMoved :: (String, String)
      -- ^ for a swapped order, the method that now runs first, and the other
    }

-- | The child that owns a port.
childOf :: Design -> Port -> Child
childOf d p = dChildren d M.! ptInst p


-- For --time-phases, which forces each phase's result.
deriving instance Generic Child
instance NFData Child
deriving instance Generic MethKind
instance NFData MethKind
deriving instance Generic Meth
instance NFData Meth
deriving instance Generic Port
instance NFData Port
deriving instance Generic Conflicts
instance NFData Conflicts
deriving instance Generic Step
instance NFData Step
deriving instance Generic Site
instance NFData Site
deriving instance Generic StepKind
instance NFData StepKind
deriving instance Generic TaskCall
instance NFData TaskCall
deriving instance Generic HwTask
instance NFData HwTask
deriving instance Generic Call
instance NFData Call
deriving instance Generic Output
instance NFData Output
deriving instance Generic Mask
instance NFData Mask
deriving instance Generic Design
instance NFData Design
deriving instance Generic Order
instance NFData Order

-- | The port serving a value-method use: by the allocation table for a
-- multi-ported method (unknown uses go to copy 0, as in 'aState').
readPortFor :: Design -> Meth -> String -> AExpr -> Maybe Port
readPortFor d mt inst use = case mtCopies mt of
    []  -> Nothing
    [p] -> Just p
    ps  -> let k = M.findWithDefault 0 use (M.findWithDefault M.empty (inst, mtName mt) (dUseCopy d))
           in listToMaybe (drop (fromInteger k) ps)

lookupMeth :: Design -> String -> String -> Maybe Meth
lookupMeth d inst m = M.lookup inst (dChildren d) >>= M.lookup m . chMethods
