{-# LANGUAGE GADTs #-}

-- | Compiler orchestration with separate observation and production effects.
--
-- Execute a plan to perform compilation, or inspect the same plan to collect
-- conservative requirements. Discovery reads observations, skips side effects,
-- and explores every explicitly represented alternative. In particular it does
-- not resolve a choice even when an observation supplied its current answer.
-- Ordinary Haskell conditionals still select one branch: choices that discovery
-- must retain need to use 'choose' or 'select'.
--
-- A skipped computation cannot supply an arbitrary value to a monadic
-- continuation. 'produce' therefore suspends that branch during discovery.
-- Use 'perform' when only () is needed, and 'independently' to keep unrelated
-- work visible even if one branch needs a value that cannot be obtained.
-- 'declareInputs' attaches an inspectable input contract to opaque work whose
-- actual reads happen inside that work, without moving those reads earlier
-- during execution.
--
-- Interpreter invariants:
--
-- * 'observe' reads in both interpretations. IO passed to 'perform', 'produce',
--   'performResult', and 'withResult' runs only during execution. Callers must
--   classify their IO honestly; the types do not enforce read-only actions.
-- * 'declareInputs', file requirements, and report facts are discovery-only.
--   In execution 'requireFiles' returns [], never a runtime lookup result.
-- * Bind distributes the continuation into every 'select' alternative.
--   'independently', 'independentlyStateT', 'runStatePlan', 'planResult',
--   'traverseState', and 'declareInputs' bound that duplication to their scope.
-- * Discovery state belongs to each branch. Independent state scopes restore
--   their actual incoming planning state; final execution aggregates stay
--   opaque. Neither skipped production nor failed reads supply invented values.
-- * Cached reads may share immutable values, but each use still visits its
--   requirements with its own conditions. A path-only visited set would lose
--   that provenance. 'abort' records an incomplete boundary and ends its branch.
module BuildPlan
    ( BuildPlan, observe, perform, produce, abort, choose, select, noAlternative, independently
    , declareInputs
    , BuildResult, performResult, planResult, withResult, requireResult, executeResultPlan
    , TraversalOrder(..), traverseState, selectStateT, independentlyStateT
    , runStatePlan, withCachedRead
    , requireFiles, reportRequirement, outputs, note, incomplete
    , executePlan, discoverDependencies
    ) where

import qualified Control.Exception as E
import Control.Monad (ap, forM_)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.State (StateT(..), runStateT)
import qualified Control.Monad.State.Strict as D
import Data.IORef
import qualified Data.Map as M

import DependencyReport
import Util (stableOrdNub)

-- | A value computed by execution. Discovery may retain an unavailable result
-- without inventing a value of its type. Only 'requireResult' can expose the
-- value to further planning; that explicitly suspends discovery if necessary.
data BuildResult a = Available a | Unavailable

instance Functor BuildResult where
    fmap f (Available value) = Available (f value)
    fmap _ Unavailable = Unavailable

instance Applicative BuildResult where
    pure = Available
    Available f <*> Available value = Available (f value)
    _ <*> _ = Unavailable

-- | The order in which execution consumes a stateful traversal's work queue.
-- Discovery keeps independent sibling scopes under either execution order.
data TraversalOrder = DepthFirst | BreadthFirst
    deriving (Eq, Show)

-- Constructors stay private: all IO enters through an explicitly classified
-- operation. In particular there is intentionally no MonadIO instance.
data BuildPlan a where
    Return :: a -> BuildPlan a
    Observe :: String -> IO b -> (b -> BuildPlan a) -> BuildPlan a
    Perform :: IO () -> BuildPlan a -> BuildPlan a
    Abort :: String -> E.SomeException -> BuildPlan a
    Select :: String -> Int -> [BuildPlan a] -> BuildPlan a
    NoAlternative :: String -> BuildPlan a
    Independent :: [BuildPlan ()] -> BuildPlan a -> BuildPlan a
    IndependentState :: s -> [s -> BuildPlan (Either e s)] ->
        (Either e s -> BuildPlan a) -> BuildPlan a
    CaptureResult :: BuildPlan b -> (BuildResult b -> BuildPlan a) -> BuildPlan a
    TraverseState :: TraversalOrder ->
        (s -> n -> BuildPlan (Either e (s, [n]))) -> s -> [n] ->
        (BuildResult (Either e s) -> BuildPlan a) -> BuildPlan a
    PerformResult :: BuildResult (IO b) -> (BuildResult b -> BuildPlan a) -> BuildPlan a
    RequireResult :: String -> BuildResult b -> (b -> BuildPlan a) -> BuildPlan a
    WithCachedRead :: Ord k => (k -> String) -> (k -> IO v) ->
        ((k -> BuildPlan v) -> BuildPlan a) -> BuildPlan a
    DeclareInputs :: BuildPlan () -> BuildPlan a -> BuildPlan a
    RequireFiles :: String -> String -> String -> [(String, FilePath)] -> [String] ->
        ([DependencyCandidate] -> BuildPlan a) -> BuildPlan a
    RequirementFact :: DependencyRequirement -> BuildPlan a -> BuildPlan a
    OutputFacts :: [FilePath] -> BuildPlan a -> BuildPlan a
    NoteFact :: String -> BuildPlan a -> BuildPlan a
    IncompleteFact :: String -> BuildPlan a -> BuildPlan a

instance Functor BuildPlan where
    fmap f plan = plan >>= return . f

instance Applicative BuildPlan where
    pure = Return
    (<*>) = ap

instance Monad BuildPlan where
    Return value >>= next = next value
    Observe label action next >>= after =
        Observe label action (\value -> next value >>= after)
    Perform action next >>= after = Perform action (next >>= after)
    Abort reason exception >>= _ = Abort reason exception
    -- Distributing bind is essential: discovery continues separately with
    -- each real branch result, including branch-local StateT state.
    Select label selected branches >>= after =
        Select label selected (map (>>= after) branches)
    NoAlternative label >>= _ = NoAlternative label
    Independent branches next >>= after = Independent branches (next >>= after)
    IndependentState initial branches next >>= after =
        IndependentState initial branches (\result -> next result >>= after)
    CaptureResult body next >>= after =
        CaptureResult body (\result -> next result >>= after)
    TraverseState order step initial seeds next >>= after =
        TraverseState order step initial seeds (\value -> next value >>= after)
    PerformResult value next >>= after =
        PerformResult value (\result -> next result >>= after)
    RequireResult label value next >>= after =
        RequireResult label value (\result -> next result >>= after)
    WithCachedRead label action body >>= after =
        WithCachedRead label action (\readInput -> body readInput >>= after)
    DeclareInputs inputs next >>= after = DeclareInputs inputs (next >>= after)
    RequireFiles owner role policy paths notes next >>= after =
        RequireFiles owner role policy paths notes (\candidates -> next candidates >>= after)
    RequirementFact fact next >>= after = RequirementFact fact (next >>= after)
    OutputFacts facts next >>= after = OutputFacts facts (next >>= after)
    NoteFact fact next >>= after = NoteFact fact (next >>= after)
    IncompleteFact fact next >>= after = IncompleteFact fact (next >>= after)

-- | Read information required by the plan. File observations must also expose
-- their input contract, normally through 'requireFiles'. Exceptions suspend
-- this branch in discovery; execution retains the action's exception behavior.
observe :: String -> IO a -> BuildPlan a
observe label action = Observe label action Return

-- | Perform a side effect whose result carries no information. Discovery
-- suppresses the action without even evaluating it, and continues with ().
-- Incidental failures retain their execution behavior; for deliberate failure
-- control flow, use 'abort' so discovery also ends that branch.
perform :: IO () -> BuildPlan ()
perform action = Perform action (Return ())

-- | Perform expensive or effectful production of a value. Discovery cannot
-- continue with that value, so records an open boundary and stops this branch.
produce :: String -> IO a -> BuildPlan a
produce label action = performResult (pure action) >>= requireResult label

-- | Fail deliberately with an exception during execution. Discovery records
-- the reason and ends this branch without forcing the exception. Enclose an
-- abortable action in 'independently' when later work is independent of it.
abort :: E.Exception e => String -> e -> BuildPlan a
abort reason exception = Abort reason (E.toException exception)

-- | Execute an action carried by an execution result. Discovery never forces
-- the action or its input handle, and returns an unavailable result. Dependencies
-- of the action must be described by the surrounding plan or 'declareInputs'.
performResult :: BuildResult (IO a) -> BuildPlan (BuildResult a)
performResult value = PerformResult value Return

-- | Compose a dependent subplan while keeping its execution result opaque.
-- An available subplan is interpreted normally during execution. Discovery
-- visits its effects in an independent scope and returns an unavailable
-- result, retaining the surrounding continuation if the subplan blocks.
-- If the subplan's inputs are unavailable, its body is skipped altogether.
-- Callers must retain its input contract in the surrounding plan, for example
-- with 'declareInputs', rather than rely on inspecting an unavailable body.
planResult :: BuildResult (BuildPlan a) -> BuildPlan (BuildResult a)
planResult (Available body) = CaptureResult body Return
planResult Unavailable = return Unavailable

-- | Consume an execution result only for its effects. Discovery continues
-- without exposing a fabricated result to the callback.
withResult :: BuildResult a -> (a -> IO ()) -> BuildPlan ()
withResult value action = performResult (fmap action value) >> return ()

-- | Demand an execution result for further planning. Unlike 'withResult', this
-- records an incomplete boundary and suspends discovery when unavailable.
requireResult :: String -> BuildResult a -> BuildPlan a
requireResult label value = RequireResult label value Return

executeResultPlan :: BuildPlan (BuildResult a) -> IO a
executeResultPlan plan = executePlan plan >>= resultValue "execution result"

resultValue :: String -> BuildResult a -> IO a
resultValue _ (Available value) = return value
resultValue label Unavailable =
    ioError (userError ("BuildPlan: unavailable " ++ label ++ " during execution"))

-- | Execution chooses the true branch (index 0) or false branch (index 1).
-- Discovery does not inspect the Boolean, and visits both alternatives.
choose :: String -> Bool -> BuildPlan a -> BuildPlan a -> BuildPlan a
choose label selected yes no = select label (if selected then 0 else 1) [yes, no]

-- | A finite choice. Execution uses the zero-based selected index; discovery
-- ignores it and retains every branch and its continuation independently.
select :: String -> Int -> [BuildPlan a] -> BuildPlan a
select = Select

-- | End a provably inapplicable alternative without supplying a result.
-- Discovery preserves earlier facts but does not continue this branch or mark
-- it incomplete. Execution must never select this alternative. Use this only
-- when the alternative has no possible candidate, with its input contract
-- already recorded; failed required reads must still report their failure.
noAlternative :: String -> BuildPlan a
noAlternative = NoAlternative

-- | Lift a choice through the state/error stack without specializing either
-- state or error types. Each alternative starts with the same incoming state.
selectStateT :: String -> Int -> [StateT s (ExceptT e BuildPlan) a] ->
                StateT s (ExceptT e BuildPlan) a
selectStateT label selected plans = StateT $ \state -> ExceptT $
    select label selected [runExceptT (runStateT plan state) | plan <- plans]

-- | Execute independent state/error actions sequentially, threading their real
-- state and stopping at the first error. Discovery instead gives every child
-- the incoming planning state, retains each child's local state and choices
-- within that child, and restores the incoming scope before continuing.
-- Errors and blocked children do not hide their siblings in discovery.
--
-- This is explicit scoped analysis, not an aggregate-state approximation.
-- Later planning must not depend on state accumulated by these siblings.
-- It is suitable for execution accumulators and visited caches when exploring
-- siblings separately remains conservative. Use 'runStatePlan' to keep the
-- final execution result opaque rather than exposing restored planning state
-- as though it were the accumulated execution state.
independentlyStateT :: [StateT s (ExceptT e BuildPlan) ()] ->
                       StateT s (ExceptT e BuildPlan) ()
independentlyStateT actions = StateT $ \initial -> ExceptT $
    fmap (fmap (\state -> ((), state))) $
        IndependentState initial (map lower actions) Return
  where
    lower action state = fmap (fmap snd) (runExceptT (runStateT action state))

-- | Describe an entire state/error action without exposing a discovery-time
-- approximation of its final state. Execution returns the actual value, state,
-- or typed error. Discovery visits the action, records typed errors, and returns
-- an unavailable result while preserving the enclosing plan's continuation.
runStatePlan :: StateT s (ExceptT e BuildPlan) a -> s ->
                BuildPlan (BuildResult (Either e (a, s)))
runStatePlan action initial = CaptureResult body Return
  where
    body = do
        result <- runExceptT (runStateT action initial)
        case result of
            Left _ -> incomplete "A stateful planning scope failed."
            Right _ -> return ()
        return result

-- | Share immutable read results within one interpretation of the body.
-- Every use still participates in the surrounding plan, so callers report
-- each dependency edge and its conditions normally. Returned values (including
-- explicit error values) are cached; thrown exceptions are not. Cache allocation
-- and mutation are private interpreter machinery, not observations of inputs.
withCachedRead :: Ord k => (k -> String) -> (k -> IO v) ->
                  ((k -> BuildPlan v) -> BuildPlan a) -> BuildPlan a
withCachedRead = WithCachedRead

-- | A group of independent effects. Unlike ordinary monadic sequencing, a
-- suspended discovery child does not hide following children or the remainder
-- of the enclosing plan. Execution remains sequential and fail-fast.
independently :: [BuildPlan ()] -> BuildPlan ()
independently branches = Independent branches (Return ())

-- | Traverse a dynamically discovered graph with explicit state. Execution
-- threads each successful step's state into the next queued step and stops at
-- the first error. The selected queue order preserves the caller's runtime
-- traversal policy.
--
-- Discovery visits every seed independently from the initial state. A step's
-- actual returned state belongs to its descendants; sibling scopes do not
-- share it or multiply each other's alternatives. The aggregate execution
-- state is therefore unavailable in discovery, rather than a made-up merge.
traverseState :: TraversalOrder ->
                 (s -> n -> BuildPlan (Either e (s, [n]))) -> s -> [n] ->
                 BuildPlan (BuildResult (Either e s))
traverseState order step initial seeds =
    TraverseState order step initial seeds Return

-- | Declare the inputs of opaque work using its shared input traversal.
-- Execution leaves this contract unevaluated: the work performs its actual
-- reads at the proper execution stage. Discovery inspects the contract as an
-- independent scope, retaining later facts even if a read fails or production
-- suspends it. Choices within the contract keep their usual discovery meaning.
--
-- This is metadata about execution, not a place to implement runtime decisions.
-- The unit result prevents analysis-only values from driving the surrounding
-- plan. In particular, use this when decoding an input would perturb execution
-- state (such as identifier interning) if moved ahead of its actual consumer.
declareInputs :: BuildPlan () -> BuildPlan ()
declareInputs inputs = DeclareInputs inputs (Return ())

-- | Probe and report input candidates only during discovery. Execution leaves
-- all requirement metadata unevaluated and supplies [] to the continuation.
-- Candidate metadata may describe additional discovery alternatives, but the
-- runtime lookup and its selected branch must not depend on this result.
requireFiles :: String -> String -> String -> [(String, FilePath)] -> [String]
             -> BuildPlan [DependencyCandidate]
requireFiles owner role policy paths notes =
    RequireFiles owner role policy paths notes Return

reportRequirement :: DependencyRequirement -> BuildPlan ()
reportRequirement fact = RequirementFact fact (Return ())

outputs :: [FilePath] -> BuildPlan ()
outputs facts = OutputFacts facts (Return ())

note :: String -> BuildPlan ()
note fact = NoteFact fact (Return ())

incomplete :: String -> BuildPlan ()
incomplete fact = IncompleteFact fact (Return ())

executePlan :: BuildPlan a -> IO a
executePlan (Return value) = return value
executePlan (Observe _ action next) = action >>= executePlan . next
executePlan (Perform action next) = action >> executePlan next
executePlan (Abort _ exception) = E.throwIO exception
executePlan (Select label selected branches)
  | selected < 0 = invalidSelection label selected
  | otherwise = case drop selected branches of
      branch : _ -> executePlan branch
      [] -> invalidSelection label selected
executePlan (NoAlternative label) =
    ioError (userError ("BuildPlan.noAlternative: selected inapplicable alternative: " ++ label))
executePlan (Independent branches next) =
    mapM_ executePlan branches >> executePlan next
executePlan (IndependentState initial branches next) = do
    result <- run initial branches
    executePlan (next result)
  where
    run state [] = return (Right state)
    run state (branch : rest) = do
        result <- executePlan (branch state)
        case result of
            Left err -> return (Left err)
            Right state' -> run state' rest
executePlan (CaptureResult body next) = do
    result <- executePlan body
    executePlan (next (Available result))
executePlan (TraverseState order step initial seeds next) = do
    result <- executeTraversal order step initial seeds
    executePlan (next (Available result))
executePlan (PerformResult value next) = do
    action <- resultValue "effect input" value
    result <- action
    executePlan (next (Available result))
executePlan (RequireResult label value next) =
    resultValue label value >>= executePlan . next
executePlan (WithCachedRead label action body) = do
    readInput <- newCachedReader label action
    executePlan (body readInput)
executePlan (DeclareInputs _ next) = executePlan next
executePlan (RequireFiles _ _ _ _ _ next) = executePlan (next [])
executePlan (RequirementFact _ next) = executePlan next
executePlan (OutputFacts _ next) = executePlan next
executePlan (NoteFact _ next) = executePlan next
executePlan (IncompleteFact _ next) = executePlan next

executeTraversal :: TraversalOrder ->
                    (s -> n -> BuildPlan (Either e (s, [n]))) -> s -> [n] ->
                    IO (Either e s)
executeTraversal order step initial seeds = run initial seeds []
  where
    -- Breadth-first work added at the rear is held in reverse queue order.
    run state [] [] = return (Right state)
    run state [] rear = run state (reverse rear) []
    run state (seed : front) rear = do
        result <- executePlan (step state seed)
        case result of
            Left err -> return (Left err)
            Right (state', children) -> case order of
                DepthFirst -> run state' (children ++ front) rear
                BreadthFirst -> run state' front (reverse children ++ rear)

newCachedReader :: Ord k => (k -> String) -> (k -> IO v) ->
                   IO (k -> BuildPlan v)
newCachedReader label action = do
    cache <- newIORef M.empty
    return $ \key -> observe (label key) $ do
        cached <- readIORef cache
        case M.lookup key cached of
            Just value -> return value
            Nothing -> do
                value <- action key
                modifyIORef' cache (M.insert key value)
                return value

invalidSelection :: String -> Int -> IO a
invalidSelection label selected =
    ioError (userError ("BuildPlan.select: invalid branch " ++ show selected ++
                       " for " ++ label))

data DiscoveryState = DiscoveryState
    { discoveredReport :: !DependencyReport
    , nextOccurrence :: !Int
    }

type Discovery = D.StateT DiscoveryState IO

-- | Explore the represented plan without executing production. Report state
-- is threaded separately from semantic traversal state, so alternatives do not
-- mutate each other's decisions or visited sets.
discoverDependencies :: String -> BuildPlan a -> IO DependencyReport
discoverDependencies mode plan = do
    let forceText :: String -> IO String
        forceText text = E.evaluate (foldr seq () text) >> return text
        update :: (DependencyReport -> DependencyReport) -> Discovery ()
        update f = D.modify' $ \state -> state
            { discoveredReport = f (discoveredReport state) }
        addIncomplete :: String -> Discovery ()
        addIncomplete reason = do
            -- Parser-derived labels and exception messages can themselves
            -- contain a deferred failure. Never let recovery store that
            -- failure in a report that will only be forced by JSON output.
            result <- D.liftIO (tryDependency (forceText reason))
            let diagnostic = case result of
                    Right text -> text
                    Left _ -> "Cannot evaluate dependency diagnostic."
            update $ \report -> report
                { dependencyIncomplete = diagnostic : dependencyIncomplete report }
        checked :: String -> IO b -> (b -> Discovery ()) -> Discovery ()
        checked label action next = do
            -- Validate the label separately: if evaluating the read also
            -- fails, its diagnostic must not re-evaluate a poisoned label.
            labelResult <- D.liftIO (tryDependency (forceText label))
            case labelResult of
                Left reason -> addIncomplete ("Cannot inspect build-plan label: " ++ reason)
                Right safeLabel -> do
                    result <- D.liftIO (tryDependency action)
                    case result of
                        Left reason -> addIncomplete (safeLabel ++ ": " ++ reason)
                        Right value -> next value
        visit :: [DependencyCondition] -> BuildPlan b -> Discovery ()
        visit conditions pending =
            -- Catch lazy continuations before inspecting the next constructor.
            -- Earlier facts remain in the explicitly threaded accumulator.
            checked "Cannot inspect build plan" (E.evaluate pending) (walk conditions)
        visitMany :: [DependencyCondition] -> [BuildPlan b] -> Discovery ()
        visitMany conditions pending =
            checked "Cannot inspect build-plan branches" (E.evaluate pending) $ \branches ->
                case branches of
                    [] -> return ()
                    branch : rest -> do
                        visit conditions branch
                        visitMany conditions rest
        walk :: [DependencyCondition] -> BuildPlan b -> Discovery ()
        walk _ (Return _) = return ()
        walk conditions (Observe label action next) =
            checked label (action >>= E.evaluate) (visit conditions . next)
        walk conditions (Perform _ next) = visit conditions next
        walk _ (Abort reason _) =
            checked "Cannot inspect build-plan abort" (forceText reason) $ \_ ->
                addIncomplete reason
        walk conditions (Select label _ branches) =
            checked "Cannot inspect build-plan choice"
                (forceText label >> E.evaluate (length branches)) $ \count -> do
                state <- D.get
                let occurrence = nextOccurrence state
                D.put (state { nextOccurrence = occurrence + 1 })
                if count == 0
                  then addIncomplete ("No alternatives in build-plan choice: " ++ label)
                  else forM_ (zip [0..] branches) $ \(branch, alternative) ->
                      visit (conditions ++
                        [DependencyCondition occurrence label branch count]) alternative
        walk _ (NoAlternative _) = return ()
        walk conditions (Independent branches next) = do
            visitMany conditions branches
            visit conditions next
        walk conditions (IndependentState initial branches next) = do
            let inspect branch = do
                    result <- branch initial
                    case result of
                        Left _ -> incomplete
                            "An independent stateful planning scope failed; sibling scopes were retained."
                        Right _ -> return ()
            visitMany conditions (map inspect branches)
            visit conditions (next (Right initial))
        walk conditions (CaptureResult body next) = do
            visit conditions body
            visit conditions (next Unavailable)
        walk conditions (TraverseState _ step initial seeds next) = do
            let descend state seed = do
                    result <- step state seed
                    case result of
                        Left _ -> incomplete
                            "A dependency traversal branch failed; independent branches were retained."
                        Right (state', children) ->
                            independently (map (descend state') children)
            visitMany conditions (map (descend initial) seeds)
            visit conditions (next Unavailable)
        walk conditions (PerformResult _ next) = visit conditions (next Unavailable)
        walk conditions (RequireResult label value next) =
            checked "Cannot inspect execution result" (E.evaluate value) $ \result ->
                case result of
                    Available actual -> visit conditions (next actual)
                    Unavailable -> addIncomplete
                        ("Dependency discovery requires unavailable execution result: " ++ label)
        walk conditions (WithCachedRead label action body) =
            checked "Cannot allocate input cache" (newCachedReader label action)
                (visit conditions . body)
        walk conditions (DeclareInputs inputs next) = do
            visit conditions inputs
            visit conditions next
        walk conditions (RequireFiles owner role policy paths notes next) =
            let candidate (kind, path)
                  | kind `elem` ["directory-tree", "include-search-directory"] =
                      directoryCandidate kind path
                  | otherwise = fileCandidate kind path
            in checked (owner ++ ": " ++ role) (mapM candidate paths) $ \candidates ->
                visit conditions (reportRequirement
                    (Requirement owner role policy candidates notes) >> next candidates)
        walk conditions (RequirementFact fact next) =
            -- A lazy binary reader can leave failures in names or candidates.
            -- Force report metadata while still inside this branch's exception
            -- boundary, not later in JSON serialization of the whole report.
            checked "Cannot inspect build-plan requirement"
                (E.evaluate (length (show fact))) $ \_ -> do
                update $ \report -> report
                    { dependencyRequirements = fact : dependencyRequirements report
                    , dependencyConditionalRequirements =
                        (conditions, fact) : dependencyConditionalRequirements report }
                visit conditions next
        walk conditions (OutputFacts facts next) =
            checked "Cannot inspect build-plan outputs"
                (mapM_ forceText facts) $ \_ -> do
                update $ \report -> report
                    { dependencyOutputs = reverse facts ++ dependencyOutputs report }
                visit conditions next
        walk conditions (NoteFact fact next) =
            checked "Cannot inspect build-plan note" (forceText fact) $ \_ -> do
                update $ \report -> report
                    { dependencyNotes = fact : dependencyNotes report }
                visit conditions next
        walk conditions (IncompleteFact fact next) =
            checked "Cannot inspect build-plan boundary" (forceText fact) $ \_ -> do
                addIncomplete fact
                visit conditions next
    (_, state) <- D.runStateT (visit [] plan) (DiscoveryState (emptyReport mode) 0)
    let report = discoveredReport state
    return report
        { dependencyRequirements = stableOrdNub (reverse (dependencyRequirements report))
        , dependencyOutputs = stableOrdNub (reverse (dependencyOutputs report))
        , dependencyNotes = stableOrdNub (reverse (dependencyNotes report))
        , dependencyIncomplete = stableOrdNub (reverse (dependencyIncomplete report))
        , dependencyConditionalRequirements =
            stableOrdNub (reverse (dependencyConditionalRequirements report))
        }
