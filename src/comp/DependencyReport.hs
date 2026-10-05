{-# LANGUAGE OverloadedStrings #-}

-- | The dependency query describes a conservative input contract, not a build
-- schedule. In particular, candidates in a source-or-object requirement are
-- alternatives whose availability and timestamps remain significant to bsc.
-- A consumer must retain the requirement policy and notes rather than treating
-- the JSON as a flat list of files that all have to exist.
module DependencyReport
    ( DependencyCandidate(..), DependencyRequirement(..), DependencyCondition(..)
    , DependencyReport(..)
    , emptyReport, fileCandidate, directoryCandidate, tryDependency
    , writeDependencyReport
    ) where

import qualified Control.Exception as E
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as BSL
import System.Directory (doesFileExist, doesDirectoryExist)

import Util (stableOrdNub)

data DependencyCandidate = Candidate
    { candidatePath :: FilePath
    , candidateKind :: String
    , candidateExists :: Bool
    } deriving (Eq, Ord, Show)

data DependencyRequirement = Requirement
    { requirementOwner :: String
    , requirementRole :: String
    , requirementPolicy :: String
    , requirementCandidates :: [DependencyCandidate]
    , requirementNotes :: [String]
    } deriving (Eq, Ord, Show)

-- A condition identifies one branch of one particular choice occurrence.
-- Branches of the same occurrence are alternatives, not simultaneous needs.
-- The occurrence distinguishes repeated choices with the same readable label.
data DependencyCondition = DependencyCondition
    { conditionOccurrence :: Int
    , conditionChoice :: String
    , conditionBranch :: Int
    , conditionBranches :: Int
    } deriving (Eq, Ord, Show)

data DependencyReport = DependencyReport
    { dependencyMode :: String
    , dependencyRequirements :: [DependencyRequirement]
    , dependencyOutputs :: [FilePath]
    , dependencyNotes :: [String]
    , dependencyIncomplete :: [String]
    , dependencyConditionalRequirements ::
        [([DependencyCondition], DependencyRequirement)]
    } deriving (Eq, Show)

emptyReport :: String -> DependencyReport
emptyReport mode = DependencyReport mode [] [] [] [] []

fileCandidate :: String -> FilePath -> IO DependencyCandidate
fileCandidate kind path = Candidate path kind <$> doesFileExist path

-- Directory candidates mean the directory tree, including names and absence,
-- not just the directory inode. They are explicit conservative boundaries for
-- searches whose precise file closure is not available from the compiler.
directoryCandidate :: String -> FilePath -> IO DependencyCandidate
directoryCandidate kind path = Candidate path kind <$> doesDirectoryExist path

-- Keep useful partial discovery when the ordinary parser or binary reader
-- rejects an input. Do not turn cancellation into a successful partial query.
tryDependency :: IO a -> IO (Either String a)
tryDependency action = E.catch (Right <$> action) handler
  where
    handler :: E.SomeException -> IO (Either String a)
    handler exception = case E.fromException exception :: Maybe E.SomeAsyncException of
        Just _ -> E.throwIO exception
        Nothing -> pure (Left (E.displayException exception))

-- Deliberately independent of the compiler's binary serializer: reports are a
-- versioned interface for external planners and must not change .bo/.ba bytes.
-- Keep the schema fields explicit so Haskell record renames do not change the
-- interface. Aeson handles JSON escaping and UTF-8; writing bytes makes the
-- report encoding independent of the process locale and text-handle encoding.
writeDependencyReport :: FilePath -> DependencyReport -> IO ()
writeDependencyReport path report =
    if path == "-" then BSL.putStr encoded else BSL.writeFile path encoded
  where
    encoded = BSL.snoc (encode value) 10
    value = object
        [ "schema" .= ("bsc-dependencies" :: String)
        , "version" .= (1 :: Int)
        , "mode" .= dependencyMode report
        , "complete" .= null (dependencyIncomplete report)
        , "requirements" .= map requirement (stableOrdNub (dependencyRequirements report))
        -- The flat requirements remain the conservative union for existing
        -- consumers. This additive field preserves the conditions under which
        -- each requirement applies; every condition in a `when` list holds,
        -- while different branches of one occurrence are alternatives. An
        -- empty `when` list preserves an unconditional occurrence even if the
        -- same requirement also occurs under a choice elsewhere.
        , "conditional_requirements" .=
            map conditional (stableOrdNub (dependencyConditionalRequirements report))
        , "potential_outputs" .= stableOrdNub (dependencyOutputs report)
        , "notes" .= stableOrdNub (dependencyNotes report)
        , "incomplete" .= stableOrdNub (dependencyIncomplete report)
        ]
    requirement value = object
        [ "owner" .= requirementOwner value
        , "role" .= requirementRole value
        , "policy" .= requirementPolicy value
        , "candidates" .= map candidate (requirementCandidates value)
        , "notes" .= requirementNotes value
        ]
    candidate value = object
        [ "path" .= candidatePath value
        , "kind" .= candidateKind value
        , "exists" .= candidateExists value
        ]
    conditional (conditions, value) = object
        [ "when" .= map condition conditions
        , "requirement" .= requirement value
        ]
    condition value = object
        [ "occurrence" .= conditionOccurrence value
        , "choice" .= conditionChoice value
        , "branch" .= conditionBranch value
        , "branches" .= conditionBranches value
        ]
