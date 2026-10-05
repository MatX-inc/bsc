{-# LANGUAGE DeriveDataTypeable #-}

-- Focused interpreter regressions. Build this through the repository's root
-- build, then run the resulting executable from the dependency testsuite.
module Main (main) where

import qualified Control.Exception as E
import qualified Control.Monad.Except as X
import qualified Control.Monad.State as S
import Control.Monad (forM, forM_, unless)
import Data.IORef
import Data.List (intercalate, isInfixOf, nub, sort)
import Data.Typeable (Typeable)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (exitFailure)
import System.IO (hClose, openTempFile)

import BuildPlan
import DependencyReport

data TestCancellation = TestCancellation deriving (Show, Typeable)

instance E.Exception TestCancellation where
    toException = E.asyncExceptionToException
    fromException = E.asyncExceptionFromException

-- Exception text can itself be lazy, just like a label derived from parsing
-- an input. Recovery must not move that failure into report serialization.
newtype TestDiagnostic = TestDiagnostic String deriving (Typeable)

instance Show TestDiagnostic where
    show (TestDiagnostic message) = message

instance E.Exception TestDiagnostic

assert :: Bool -> String -> IO ()
assert condition message = unless condition (ioError (userError message))

fact :: String -> DependencyRequirement
fact name = Requirement name "test-input" "required"
    [Candidate name "source" True] []

record :: String -> BuildPlan ()
record = reportRequirement . fact

type Stateful s a = S.StateT s (X.ExceptT String BuildPlan) a

liftPlan :: BuildPlan a -> Stateful s a
liftPlan = S.lift . S.lift

owners :: DependencyReport -> [String]
owners = sort . map requirementOwner . dependencyRequirements

expectOwners :: [String] -> DependencyReport -> IO ()
expectOwners expected report = assert (owners report == sort expected)
    ("expected inputs " ++ show expected ++ ", got " ++ show (owners report))

expectOutputs :: [String] -> DependencyReport -> IO ()
expectOutputs expected report =
    assert (sort (dependencyOutputs report) == sort expected)
        ("unexpected outputs: " ++ show (dependencyOutputs report))

hasIncomplete :: String -> DependencyReport -> Bool
hasIncomplete text = any (isInfixOf text) . dependencyIncomplete

expectWritableIncomplete :: DependencyReport -> IO ()
expectWritableIncomplete report = do
    directory <- getTemporaryDirectory
    E.bracket
        (do (path, handle) <- openTempFile directory "bsc-build-plan-report"
            hClose handle
            return path)
        removeFile $ \path -> do
            writeDependencyReport path report
            contents <- readFile path
            assert ("\"complete\":false" `isInfixOf` contents)
                "recovered report did not serialize as incomplete"
            _ <- E.evaluate (length contents)
            return ()

tests :: [(String, IO ())]
tests =
    [ ("discovery unions both choices for either observed answer", do
        forM_ [False, True] $ \answer -> do
            report <- discoverDependencies "test" $ do
                selected <- observe "answer" (return answer)
                choose "observed condition" selected (record "yes") (record "no")
            expectOwners ["yes", "no"] report
        return ())
    , ("discovery does not evaluate the selector", do
        report <- discoverDependencies "test" $
            choose "unresolved condition" (error "selector forced")
                (record "yes") (record "no")
        expectOwners ["yes", "no"] report
        assert (null (dependencyIncomplete report)) "selector was evaluated")
    , ("each real alternative result drives its own continuation", do
        report <- discoverDependencies "test" $ do
            name <- select "input candidate" 0 [return "first", return "second"]
            record (name ++ "-dependency")
        expectOwners ["first-dependency", "second-dependency"] report
        let guarded = dependencyConditionalRequirements report
            guards = map fst guarded
        assert (map length guards == [1, 1]) "missing choice guards"
        assert (map (conditionBranch . head) guards == [0, 1])
            "alternatives were not kept distinct"
        assert (length (nub (map (conditionOccurrence . head) guards)) == 1)
            "one choice has different occurrence identifiers")
    , ("inapplicable alternatives preserve facts without inventing a result", do
        let plan = do
                value <- select "available candidates" 0
                    [ return "available"
                    , record "empty-search" >> noAlternative "no candidate"
                    ]
                record (value ++ "-dependency")
                return value
        actual <- executePlan plan
        assert (actual == "available") "execution evaluated an inapplicable alternative"
        report <- discoverDependencies "test" $ do
            independently [plan >> return (), record "independent"]
            record "after-choice"
        expectOwners ["available-dependency", "empty-search", "independent", "after-choice"] report
        assert (null (dependencyIncomplete report))
            "inapplicable alternative was mistaken for a failed required read")
    , ("inapplicable alternatives reject accidental execution", do
        result <- E.try (executePlan (noAlternative "absent input" :: BuildPlan ()))
            :: IO (Either E.IOException ())
        case result of
            Left err -> assert ("absent input" `isInfixOf` show err)
                "inapplicable alternative error lost its label"
            Right _ -> assert False "inapplicable alternative supplied a result")
    , ("nested and repeated choices preserve their own conditions", do
        report <- discoverDependencies "test" $ independently
            [ choose "same label" True
                (choose "nested" True (record "a") (record "b"))
                (record "c")
            , choose "same label" False (record "d") (record "e")
            ]
        expectOwners ["a", "b", "c", "d", "e"] report
        let guards = dependencyConditionalRequirements report
            conditionsFor name = concat
                [conditions | (conditions, requirement) <- guards,
                              requirementOwner requirement == name]
        assert (length (conditionsFor "a") == 2) "nested condition lost"
        assert (length (conditionsFor "c") == 1) "unrelated condition leaked"
        assert (conditionOccurrence (head (conditionsFor "c")) /=
                conditionOccurrence (head (conditionsFor "d")))
            "repeated choice labels were conflated")
    , ("unconditional occurrences survive conditional duplicate requirements", do
        report <- discoverDependencies "test" $ do
            record "shared"
            choose "optional use" True (record "shared") (record "alternative")
        expectOwners ["shared", "alternative"] report
        let sharedConditions =
                [conditions | (conditions, requirement) <-
                                  dependencyConditionalRequirements report,
                              requirementOwner requirement == "shared"]
        assert (map length sharedConditions == [0, 1])
            "unconditional requirement occurrence was lost")
    , ("report deduplication preserves the first occurrence order", do
        report <- discoverDependencies "test" $ do
            mapM_ record ["second", "first", "second", "third", "first"]
            outputs ["second.o", "first.o", "second.o", "third.o", "first.o"]
            mapM_ note ["second", "first", "second"]
            mapM_ incomplete ["second", "first", "second"]
            choose "ordered alternatives" True
                (record "second" >> record "second")
                (record "first" >> record "first")
        assert (map requirementOwner (dependencyRequirements report) ==
                ["second", "first", "third"])
            "requirement first occurrences were reordered"
        assert (dependencyOutputs report == ["second.o", "first.o", "third.o"])
            "output first occurrences were reordered"
        assert (dependencyNotes report == ["second", "first"] &&
                dependencyIncomplete report == ["second", "first"])
            "note or incomplete first occurrences were reordered"
        let guarded = dependencyConditionalRequirements report
        assert (map (requirementOwner . snd) guarded ==
                ["second", "first", "third", "second", "first"] &&
                map (length . fst) guarded == [0, 0, 0, 1, 1])
            "conditional first occurrences or distinct provenance were lost")
    , ("perform suppresses even an unevaluated action and continues", do
        report <- discoverDependencies "test" $ do
            perform (error "write action forced")
            record "after-write"
        expectOwners ["after-write"] report
        assert (null (dependencyIncomplete report)) "write action was forced")
    , ("potential outputs are retained across alternative production paths", do
        report <- discoverDependencies "test" $
            choose "output alternatives" True
                (outputs ["first.o"] >> perform (error "first write forced"))
                (outputs ["second.o"] >> perform (error "second write forced"))
        expectOutputs ["first.o", "second.o"] report)
    , ("produce does not evaluate work or invent its result", do
        report <- discoverDependencies "test" $ do
            record "before-production"
            value <- produce "expensive compilation"
                (error "production action forced" :: IO String)
            record value
        expectOwners ["before-production"] report
        assert (hasIncomplete "expensive compilation" report)
            "missing suspended production boundary"
        assert (not (hasIncomplete "production action forced" report))
            "production action was forced")
    , ("independent work survives a suspended sibling", do
        report <- discoverDependencies "test" $ do
            independently
                [ produce "blocked value" (return "made-up") >>= record
                , record "later-sibling"
                ]
            record "after-group"
        expectOwners ["later-sibling", "after-group"] report)
    , ("breadth-first execution preserves queue order and chosen children", do
        visits <- newIORef ([] :: [String])
        let append name = perform (modifyIORef' visits (++ [name]))
            step name = do
                append name
                case name of
                    "root-a" -> choose "execution children" True
                        (return ["a", "b"]) (return ["wrong"])
                    "root-b" -> return ["c"]
                    "a" -> return ["a-child"]
                    "b" -> return ["b-child"]
                    "c" -> return ["c-child"]
                    _ -> return []
        value <- executePlan $ do
            breadthFirst step ["root-a", "root-b"]
            append "after-traversal"
            return (42 :: Int)
        seen <- readIORef visits
        assert (seen == ["root-a", "root-b", "a", "b", "c",
                         "a-child", "b-child", "c-child", "after-traversal"] &&
                value == 42) ("unexpected breadth-first execution: " ++ show seen))
    , ("breadth-first discovery retains each alternative child provenance", do
        let step name = do
                record name
                case name of
                    "root" -> select "discovery children" (error "selector forced")
                        [return ["left"], return ["right"]]
                    "left" -> return ["left-child"]
                    "right" -> return ["right-child"]
                    _ -> return []
        report <- discoverDependencies "test" $ do
            breadthFirst step ["root", "independent-root"]
            record "after-traversal"
        expectOwners ["root", "left", "right", "left-child", "right-child",
                      "independent-root", "after-traversal"] report
        let guardsFor name =
                [conditions | (conditions, requirement) <-
                                  dependencyConditionalRequirements report,
                              requirementOwner requirement == name]
        case (guardsFor "left", guardsFor "right") of
            ([[left]], [[right]]) -> do
                assert (conditionOccurrence left == conditionOccurrence right &&
                        conditionBranch left == 0 && conditionBranch right == 1)
                    "alternative children lost their common choice occurrence"
                assert (guardsFor "left-child" == [[left]] &&
                        guardsFor "right-child" == [[right]])
                    "descendants lost their ancestor choice conditions"
            _ -> assert False "alternative children lost their choice guards"
        assert (all ((== [[]]) . guardsFor)
                    ["root", "independent-root", "after-traversal"])
            "child choice conditions leaked outside their branch"
        assert (null (dependencyIncomplete report)) "discovery evaluated the selector")
    , ("breadth-first discovery retains siblings after a suspended step", do
        let step name = do
                record name
                case name of
                    "root" -> return ["blocked", "later-child"]
                    "blocked" -> produce "blocked children"
                        (error "child production forced" :: IO [String])
                    _ -> return []
        report <- discoverDependencies "test" $ do
            breadthFirst step ["root", "later-root"]
            record "after-traversal"
        expectOwners ["root", "blocked", "later-child", "later-root",
                      "after-traversal"] report
        assert (hasIncomplete "blocked children" report &&
                not (hasIncomplete "child production forced" report))
            "suspended traversal executed production or lost its boundary")
    , ("breadth-first execution fails before later queued work", do
        visits <- newIORef ([] :: [String])
        let step name = do
                perform (modifyIORef' visits (++ [name]))
                if name == "bad"
                    then observe "failed step" (ioError (userError "step failed"))
                    else return ["child"]
        result <- E.try (executePlan $ do
            breadthFirst step ["root", "bad", "later-root"]
            perform (modifyIORef' visits (++ ["after-traversal"])))
            :: IO (Either IOError ())
        seen <- readIORef visits
        case result of
            Left _ -> assert (seen == ["root", "bad"])
                ("execution continued after failure: " ++ show seen)
            Right _ -> assert False "execution swallowed traversal failure")
    , ("input contract observations run only during discovery", do
        reads <- newIORef (0 :: Int)
        let plan = do
                declareInputs $ do
                    name <- observe "input metadata" $ do
                        modifyIORef' reads (+ 1)
                        return "declared-input"
                    record name
                return (42 :: Int)
        value <- executePlan plan
        executionReads <- readIORef reads
        assert (value == 42 && executionReads == 0)
            "execution evaluated an input contract"
        report <- discoverDependencies "test" plan
        discoveryReads <- readIORef reads
        assert (discoveryReads == 1) "discovery skipped the input observation"
        expectOwners ["declared-input"] report)
    , ("execution does not force the input contract itself", do
        value <- executePlan $ do
            declareInputs (error "input contract forced")
            return (42 :: Int)
        assert (value == 42) "execution did not continue after the contract")
    , ("suspended input contracts retain surrounding facts", do
        report <- discoverDependencies "test" $ do
            record "before-contract"
            declareInputs $ do
                record "declared-input"
                perform (error "contract write forced")
                name <- produce "contract production"
                    (error "contract production forced" :: IO String)
                record name
            record "after-contract"
        expectOwners ["before-contract", "declared-input", "after-contract"] report
        assert (hasIncomplete "contract production" report)
            "missing suspended contract boundary"
        assert (not (hasIncomplete "forced" report))
            "contract executed a write or production")
    , ("input contract alternatives do not guard surrounding facts", do
        report <- discoverDependencies "test" $ do
            declareInputs $
                choose "contract alternatives" (error "contract selector forced")
                    (record "first-input") (record "second-input")
            record "after-contract"
        expectOwners ["first-input", "second-input", "after-contract"] report
        let guardsFor name =
                [conditions | (conditions, requirement) <-
                                  dependencyConditionalRequirements report,
                              requirementOwner requirement == name]
        assert (map length (guardsFor "first-input") == [1] &&
                map length (guardsFor "second-input") == [1])
            "input contract lost its alternative guards"
        assert (guardsFor "after-contract" == [[]])
            "contract alternatives leaked into surrounding facts")
    , ("observation failure suspends one alternative and retains facts", do
        report <- discoverDependencies "test" $
            choose "read alternatives" True
                (do record "before-error"
                    value <- observe "bad read" (ioError (userError "unreadable"))
                    record value)
                (record "other-alternative")
        expectOwners ["before-error", "other-alternative"] report
        assert (hasIncomplete "bad read: " report) "missing read failure")
    , ("lazy observation failure is caught inside the read boundary", do
        report <- discoverDependencies "test" $ independently
            [ observe "lazy read" (return (error "bad value" :: String)) >>= record
            , record "safe-sibling"
            ]
        expectOwners ["safe-sibling"] report
        assert (hasIncomplete "lazy read: " report) "lazy failure escaped")
    , ("lazy observation labels do not poison the recovered report", do
        forM_ ["read " ++ error "bad label tail",
               "read " ++ [error "bad label character"]] $ \label -> do
            report <- discoverDependencies "test" $ independently
                [ do record "before-label"
                     -- An include lookup can fail while evaluating the same
                     -- filename thunk carried by its observation label.
                     observe label (E.evaluate (foldr seq () label))
                     record "after-label"
                , record "safe-sibling"
                ]
            expectOwners ["before-label", "safe-sibling"] report
            expectWritableIncomplete report
            assert (hasIncomplete "Cannot inspect build-plan label" report)
                "missing stable label failure boundary")
    , ("lazy characters in report facts are caught before serialization", do
        let badText = "prefix " ++ [error "bad report character"]
            cases =
                [ ("choice", choose badText True (record "bad-choice") (record "other-choice"))
                , ("outputs", outputs [badText] >> record "after-output")
                , ("note", note badText >> record "after-note")
                , ("boundary", incomplete badText >> record "after-boundary")
                ]
        forM_ cases $ \(name, plan) -> do
            report <- discoverDependencies "test" $ independently
                [record "before-fact" >> plan, record "safe-sibling"]
            expectOwners ["before-fact", "safe-sibling"] report
            expectWritableIncomplete report
            assert (hasIncomplete ("Cannot inspect build-plan " ++ name) report)
                ("missing failure boundary for " ++ name))
    , ("lazy exception text receives a serializable fallback diagnostic", do
        report <- discoverDependencies "test" $ independently
            [ do record "before-error"
                 observe "failed read" $ E.throwIO $ TestDiagnostic
                     ("diagnostic " ++ [error "bad diagnostic character"])
                 record "after-error"
            , record "safe-sibling"
            ]
        expectOwners ["before-error", "safe-sibling"] report
        assert (hasIncomplete "Cannot evaluate dependency diagnostic" report)
            "missing fallback for an unevaluable diagnostic"
        expectWritableIncomplete report)
    , ("lazy production labels receive a serializable fallback diagnostic", do
        report <- discoverDependencies "test" $ independently
            [ do record "before-production"
                 produce ("production " ++ error "bad production label")
                     (error "production action forced" :: IO ())
                 record "after-production"
            , record "safe-sibling"
            ]
        expectOwners ["before-production", "safe-sibling"] report
        assert (hasIncomplete "Cannot evaluate dependency diagnostic" report)
            "missing fallback for an unevaluable production label"
        expectWritableIncomplete report)
    , ("execution chooses one branch and performs production", do
        writes <- newIORef ([] :: [String])
        let append value = perform (modifyIORef' writes (++ [value]))
        result <- executePlan $ do
            choose "first choice" True (append "yes") (append "wrong-no")
            choose "second choice" False (append "wrong-yes") (append "no")
            produce "work" (modifyIORef' writes (++ ["produced"]) >> return (42 :: Int))
        seen <- readIORef writes
        assert (seen == ["yes", "no", "produced"] && result == 42)
            ("unexpected execution: " ++ show (seen, result)))
    , ("execution propagates observation failures", do
        result <- E.try (executePlan
            (observe "read" (ioError (userError "execution read failed"))))
            :: IO (Either IOError ())
        case result of
            Left _ -> return ()
            Right _ -> assert False "execution swallowed the read failure")
    , ("discovery propagates standard asynchronous exceptions", do
        result <- E.try (discoverDependencies "test"
            (observe "cancelled read" (E.throwIO E.ThreadKilled) :: BuildPlan ()))
            :: IO (Either E.AsyncException DependencyReport)
        case result of
            Left E.ThreadKilled -> return ()
            _ -> assert False "discovery swallowed ThreadKilled")
    , ("discovery propagates custom asynchronous exceptions", do
        result <- E.try (discoverDependencies "test"
            (observe "cancelled read" (E.throwIO TestCancellation) :: BuildPlan ()))
            :: IO (Either TestCancellation DependencyReport)
        case result of
            Left TestCancellation -> return ()
            Right _ -> assert False "discovery swallowed a custom cancellation")
    , ("discovery propagates cancellation while evaluating labels", do
        result <- E.try (discoverDependencies "test"
            (observe (E.throw E.ThreadKilled) (return ()) :: BuildPlan ()))
            :: IO (Either E.AsyncException DependencyReport)
        case result of
            Left E.ThreadKilled -> return ()
            _ -> assert False "label recovery swallowed ThreadKilled")
    , ("discovery propagates cancellation while evaluating diagnostics", do
        result <- E.try (discoverDependencies "test"
            (observe "cancelled diagnostic"
                (E.throwIO (TestDiagnostic (E.throw TestCancellation))) :: BuildPlan ()))
            :: IO (Either TestCancellation DependencyReport)
        case result of
            Left TestCancellation -> return ()
            Right _ -> assert False "diagnostic recovery swallowed custom cancellation")
    , ("file requirements retain present absent and directory candidates", do
        directory <- getTemporaryDirectory
        E.bracket
            (do (path, handle) <- openTempFile directory "bsc-build-plan-test"
                hClose handle
                return path)
            removeFile $ \path -> do
                report <- discoverDependencies "test" $ do
                    _ <- requireFiles "owner" "input-search" "one-of"
                        [("source", path), ("source", path ++ ".missing"),
                         ("directory-tree", directory)] []
                    return ()
                case dependencyRequirements report of
                    [requirement] -> do
                        assert (map candidateExists (requirementCandidates requirement)
                                == [True, False, True]) "candidate presence changed"
                        assert (requirementPolicy requirement == "one-of")
                            "alternative policy changed"
                    _ -> assert False "file requirement not recorded")

    , ("execution results pass values between effects without hidden state", do
        let plan = do
                first <- performResult (pure (return (20 :: Int)))
                second <- performResult ((\x -> return (x * 2)) <$> first)
                performResult ((\x -> return (x + 2)) <$> second)
        value <- executeResultPlan plan
        assert (value == 42) "execution result pipeline lost its values")
    , ("available subplans execute normally and expose independent discovery facts", do
        visits <- newIORef ([] :: [String])
        let body = do
                name <- select "subplan choice" 1 [return "first", return "second"]
                record name
                perform (modifyIORef' visits (++ [name]))
                return name
        actual <- executeResultPlan (planResult (pure body))
        seen <- readIORef visits
        assert (actual == "second" && seen == ["second"])
            "subplan execution lost its selected result or effects"
        report <- discoverDependencies "test" $ do
            result <- planResult (pure body)
            withResult result (\_ -> error "subplan result exposed")
            record "after-subplan"
        expectOwners ["first", "second", "after-subplan"] report
        let afterConditions = [conditions | (conditions, requirement) <-
                dependencyConditionalRequirements report,
                requirementOwner requirement == "after-subplan"]
        assert (afterConditions == [[]] && null (dependencyIncomplete report))
            "subplan alternatives leaked into the surrounding continuation")
    , ("unavailable subplans skip their body and retain the declared inputs", do
        report <- discoverDependencies "test" $ do
            input <- performResult (pure (error "input production forced" :: IO Int))
            declareInputs (record "subplan-input")
            result <- planResult ((\_ -> error "unavailable subplan forced" :: BuildPlan ()) <$> input)
            withResult result (\_ -> error "unavailable subplan result exposed")
            record "after-subplan"
        expectOwners ["subplan-input", "after-subplan"] report
        assert (null (dependencyIncomplete report))
            "unavailable subplan was inspected despite its input contract")
    , ("blocked subplans preserve their earlier facts and outer continuation", do
        report <- discoverDependencies "test" $ do
            result <- planResult $ pure $ do
                record "before-production"
                produce "subplan production" (error "production forced" :: IO Int)
            withResult result (\_ -> error "blocked subplan result exposed")
            record "after-subplan"
        expectOwners ["before-production", "after-subplan"] report
        assert (hasIncomplete "subplan production" report &&
                not (hasIncomplete "production forced" report))
            "subplan capture lost its blocked production boundary")
    , ("discovery skips execution results without forcing handles or callbacks", do
        report <- discoverDependencies "test" $ do
            value <- performResult (error "effect handle forced" :: BuildResult (IO Int))
            next <- performResult ((\_ -> error "stage forced" :: IO Int) <$> value)
            withResult next (\_ -> error "consumer forced")
            record "after-stages"
        expectOwners ["after-stages"] report
        assert (null (dependencyIncomplete report)) "suppressed stages were inspected")
    , ("demanding an execution result marks an explicit discovery boundary", do
        report <- discoverDependencies "test" $ independently
            [ do value <- performResult (pure (return ("output" :: String)))
                 actual <- requireResult "generated filename" value
                 record actual
            , record "independent-input"
            ]
        expectOwners ["independent-input"] report
        assert (hasIncomplete "generated filename" report)
            "unavailable result was fabricated or silently discarded")
    , ("stateful execution returns accumulated state in the requested order", do
        let step seen name = return (Right (seen ++ [name], children name)
                :: Either String ([String], [String]))
            children "a" = ["a1", "a2"]
            children "b" = ["b1"]
            children _ = []
        forM_ [(DepthFirst, ["a", "a1", "a2", "b", "b1"]),
               (BreadthFirst, ["a", "b", "a1", "a2", "b1"])] $ \(order, expected) -> do
            result <- executeResultPlan (traverseState order step [] ["a", "b"])
            assert (result == Right expected) ("incorrect state traversal: " ++ show result))
    , ("stateful execution stops at the first error", do
        seen <- newIORef ([] :: [String])
        let step state name = do
                perform (modifyIORef' seen (++ [name]))
                return (if name == "bad" then Left "failure"
                        else Right (state + 1, []) :: Either String (Int, [String]))
        result <- executeResultPlan (traverseState DepthFirst step 0 ["ok", "bad", "later"])
        visits <- readIORef seen
        assert (result == Left "failure" && visits == ["ok", "bad"])
            "stateful traversal continued past an error")
    , ("discovery isolates sibling state and preserves actual alternative descendants", do
        let step state name = case name of
                "root" -> select "state alternatives" (error "selector forced")
                    [return (Right (1, ["child"])), return (Right (2, ["child"]))]
                _ -> do
                    record (name ++ show state)
                    return (Right (state + 10, []) :: Either String (Int, [String]))
        report <- discoverDependencies "test" $ do
            result <- traverseState DepthFirst step 0 ["root", "sibling"]
            withResult result (\_ -> error "aggregate state inspected")
            record "after-traversal"
        expectOwners ["child1", "child2", "sibling0", "after-traversal"] report
        let guards name = [cs | (cs, requirement) <- dependencyConditionalRequirements report,
                                requirementOwner requirement == name]
        assert (map length (guards "child1") == [1] &&
                map length (guards "child2") == [1] &&
                guards "sibling0" == [[]] && guards "after-traversal" == [[]])
            "state alternatives escaped their child scopes"
        assert (null (dependencyIncomplete report)) "valid state traversal marked incomplete")
    , ("stateful discovery retains siblings after errors and blocked production", do
        let step state name = do
                record name
                case name of
                    "root" -> return (Right (state, ["bad", "blocked", "later"]))
                    "bad" -> return (Left "missing input")
                    "blocked" -> produce "blocked state" (error "production forced")
                    _ -> return (Right (state, []) :: Either String ((), [String]))
        report <- discoverDependencies "test" $ do
            _ <- traverseState BreadthFirst step () ["root", "other-root"]
            record "after-traversal"
        expectOwners ["root", "bad", "blocked", "later", "other-root", "after-traversal"] report
        assert (hasIncomplete "blocked state" report && not (hasIncomplete "production forced" report))
            "blocked state did not retain the correct boundary")
    , ("generic state choices retain branch-local state", do
        let plan = do
                selectStateT "state choice" (error "selector forced") [S.put 1, S.put 2]
                    :: S.StateT Int (X.ExceptT String BuildPlan) ()
                value <- S.get
                S.lift (S.lift (record (show value)))
        report <- discoverDependencies "test" (X.runExceptT (S.runStateT plan 0))
        expectOwners ["1", "2"] report)
    , ("recursive state scopes preserve ancestors and terminate cycles", do
        let children "root" = ["left", "right"]
            children "left" = ["root", "leaf"]
            children "right" = ["leaf"]
            children _ = []
            walk :: String -> Stateful [String] ()
            walk name = do
                known <- S.get
                unless (name `elem` known) $ do
                    liftPlan (record (name ++ "@" ++ intercalate "/" known))
                    S.put (known ++ [name])
                    independentlyStateT (map walk (children name))
        actual <- executeResultPlan (runStatePlan (walk "root") [])
        assert (actual == Right ((), ["root", "left", "leaf", "right"]))
            ("recursive execution state changed: " ++ show actual)
        report <- discoverDependencies "test" $ do
            result <- runStatePlan (walk "root") []
            withResult result (\_ -> error "recursive aggregate forced")
            record "after-recursion"
        expectOwners ["root@", "left@root", "leaf@root/left", "right@root",
                      "leaf@root/right", "after-recursion"] report
        assert (null (dependencyIncomplete report))
            "valid recursive scopes were marked incomplete")
    , ("independent state choices do not multiply sibling continuations", do
        let child :: Int -> Stateful Int ()
            child n = do
                selectStateT ("child " ++ show n) 1 [S.put (2*n), S.put (2*n+1)]
                value <- S.get
                liftPlan (record ("value:" ++ show value))
            action :: Stateful Int ()
            action = do
                independentlyStateT (map child [0..11])
                liftPlan (record "after-group")
        actual <- executeResultPlan (runStatePlan action 0)
        assert (actual == Right ((), 23)) "execution failed to retain selected sibling state"
        report <- discoverDependencies "test" (runStatePlan action 0)
        expectOwners ("after-group" : ["value:" ++ show n | n <- [0..23 :: Int]]) report
        let guarded = dependencyConditionalRequirements report
            childGuards = [conditions | (conditions, requirement) <- guarded,
                               requirementOwner requirement /= "after-group"]
            parentGuards = [conditions | (conditions, requirement) <- guarded,
                                requirementOwner requirement == "after-group"]
        assert (length childGuards == 24 && all ((== 1) . length) childGuards &&
                length (nub (map (conditionOccurrence . head) childGuards)) == 12 &&
                parentGuards == [[]])
            "sibling choices multiplied or escaped their independent scope")
    , ("state scopes fail fast in execution and retain discovery siblings", do
        visits <- newIORef ([] :: [String])
        let recordVisit name = liftPlan $ do
                perform (modifyIORef' visits (++ [name]))
                record name
            action :: Stateful Int ()
            action = do
                S.put 42
                independentlyStateT
                    [ recordVisit "first" >> S.modify (+ 1)
                    , recordVisit "bad" >> X.throwError "failure"
                    , do recordVisit "blocked"
                         liftPlan (produce "blocked scope" (error "production forced" :: IO ()))
                    , do state <- S.get
                         recordVisit ("last:" ++ show state)
                    ]
                recordVisit "after-group"
        actual <- executeResultPlan (runStatePlan action 0)
        seen <- readIORef visits
        assert (actual == Left "failure" && seen == ["first", "bad"])
            "execution continued after a typed child error"
        report <- discoverDependencies "test" $ do
            _ <- runStatePlan action 0
            record "after-run"
        expectOwners ["first", "bad", "blocked", "last:42", "after-group", "after-run"] report
        assert (hasIncomplete "independent stateful planning scope failed" report &&
                hasIncomplete "blocked scope" report &&
                not (hasIncomplete "production forced" report))
            "child failure or suspension was lost during discovery")
    , ("state-plan typed errors retain surrounding discovery work", do
        let action :: Stateful Int ()
            action = liftPlan (record "before-error") >> X.throwError "root failure"
        actual <- executeResultPlan (runStatePlan action 5)
        assert (actual == Left "root failure") "state-plan runner lost its typed error"
        report <- discoverDependencies "test" $ do
            result <- runStatePlan action 5
            withResult result (\_ -> error "failed result forced")
            record "after-error"
        expectOwners ["before-error", "after-error"] report
        assert (hasIncomplete "stateful planning scope failed" report)
            "typed state-plan error did not mark discovery incomplete")
    , ("state-plan capture never exposes restored scope as final aggregate", do
        let action :: Stateful Int String
            action = do
                S.put 10
                independentlyStateT [S.modify (+ 1), S.modify (* 2)]
                return "result"
        actual <- executeResultPlan (runStatePlan action 0)
        assert (actual == Right ("result", 22)) "execution aggregate was not accumulated"
        report <- discoverDependencies "test" $ do
            result <- runStatePlan action 0
            withResult result (\_ -> error "aggregate consumer forced")
            record "after-run"
            independently
                [ requireResult "final state aggregate" result >>= record . show
                , record "independent-input"
                ]
        expectOwners ["after-run", "independent-input"] report
        assert (hasIncomplete "final state aggregate" report)
            "restored planning state was presented as an execution result")
    , ("state-plan capture retains continuation after blocked production", do
        let action :: Stateful Int String
            action = do
                liftPlan (record "before-production")
                liftPlan (produce "state-plan production" (error "production forced" :: IO String))
        report <- discoverDependencies "test" $ do
            result <- runStatePlan action 0
            withResult result (\_ -> error "blocked consumer forced")
            record "after-production"
        expectOwners ["before-production", "after-production"] report
        assert (hasIncomplete "state-plan production" report &&
                not (hasIncomplete "production forced" report))
            "state-plan capture lost the production boundary")
    , ("cached reads are shared within each interpretation and fresh between runs", do
        reads <- newIORef (0 :: Int)
        let load _ = atomicModifyIORef' reads (\n -> (n + 1, n + 1))
            plan = withCachedRead id load $ \readInput -> do
                a <- readInput "same"
                b <- readInput "same"
                return (a, b)
        first <- executePlan plan
        second <- executePlan plan
        _ <- discoverDependencies "test" plan
        count <- readIORef reads
        assert (first == (1, 1) && second == (2, 2) && count == 3)
            "read cache leaked across interpretations or repeated a read")
    , ("cached reads retain every dependency occurrence and its conditions", do
        reads <- newIORef (0 :: Int)
        let load name = modifyIORef' reads (+ 1) >> return name
        report <- discoverDependencies "test" $ withCachedRead id load $ \readInput ->
            select "cached alternatives" 0
                [readInput "shared" >>= record, readInput "shared" >>= record]
        count <- readIORef reads
        let conditions = map fst (dependencyConditionalRequirements report)
        assert (count == 1 && map (conditionBranch . head) conditions == [0, 1])
            "cache lost provenance or repeated decoding")
    , ("cached reads retry thrown failures rather than caching them", do
        reads <- newIORef (0 :: Int)
        let load name = do
                n <- atomicModifyIORef' reads (\x -> (x + 1, x + 1))
                if n == 1 then ioError (userError "first read failed") else return name
        report <- discoverDependencies "test" $ withCachedRead id load $ \readInput ->
            independently [readInput "input" >>= record,
                           readInput "input" >>= record,
                           readInput "input" >>= record]
        count <- readIORef reads
        expectOwners ["input"] report
        assert (count == 2 && hasIncomplete "first read failed" report)
            "failed read was cached or successful read was not shared")
    , ("cached reads propagate asynchronous cancellation", do
        let plan = withCachedRead id (\_ -> E.throwIO E.ThreadKilled) $ \readInput ->
                readInput "cancelled" :: BuildPlan ()
        result <- E.try (discoverDependencies "test" plan)
            :: IO (Either E.AsyncException DependencyReport)
        case result of
            Left E.ThreadKilled -> return ()
            _ -> assert False "cached read swallowed asynchronous cancellation")
    ]

main :: IO ()
main = do
    results <- forM tests $ \(name, action) -> do
        result <- tryDependency action
        case result of
            Right () -> putStrLn ("PASS: BuildPlan: " ++ name) >> return True
            Left reason -> do
                putStrLn ("FAIL: BuildPlan: " ++ name ++ " (" ++ reason ++ ")")
                return False
    unless (and results) exitFailure
