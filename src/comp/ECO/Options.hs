-- ECO command-line parsing stays separate from ordinary flag decoding.
-- Parse only ordinary Strings so replay can restore the intern table first.
module ECO.Options (ECOOptions(..), ECOInvocation(..), defaultECOOptions,
                   parseECOOptions, invocationOptions, ecoEnabled) where

import Warmup ()

import Data.List (isPrefixOf)

data ECOOptions = ECOOptions
    { ecoCapture :: Maybe FilePath
    , ecoGraphDir :: Maybe FilePath
    , ecoBackendPlan :: Maybe FilePath
    , ecoReplay :: Maybe FilePath
    , ecoOutput :: Maybe FilePath
    , ecoAdapt :: Maybe FilePath
    } deriving (Eq, Show)

defaultECOOptions :: ECOOptions
defaultECOOptions = ECOOptions Nothing Nothing Nothing Nothing Nothing Nothing

ecoEnabled :: ECOOptions -> Bool
ecoEnabled o = o /= defaultECOOptions

-- CLI modes carry their required paths directly. This transient value contains
-- only ordinary Strings; constructing it cannot touch the compiler interner.
-- ECOOptions remains the invocation-only runtime projection stored in Flags.
data ECOInvocation
    = CompileInvocation (Maybe FilePath) (Maybe FilePath) [String]
      -- capture directory, graph directory, ordinary compiler arguments
    | ReplayInvocation FilePath FilePath (Maybe FilePath) (Maybe FilePath)
      -- capture, output directory, keep plan, graph directory
    | AdaptInvocation FilePath FilePath (Maybe FilePath)
      -- capture, output directory, graph directory
    deriving (Eq, Show)

invocationOptions :: ECOInvocation -> ECOOptions
invocationOptions invocation = case invocation of
    CompileInvocation capture graph _ -> defaultECOOptions
        { ecoCapture = capture, ecoGraphDir = graph }
    ReplayInvocation capture output plan graph -> defaultECOOptions
        { ecoReplay = Just capture, ecoOutput = Just output,
          ecoBackendPlan = plan, ecoGraphDir = graph }
    AdaptInvocation capture output graph -> defaultECOOptions
        { ecoAdapt = Just capture, ecoOutput = Just output, ecoGraphDir = graph }

parseECOOptions :: [String] -> Either String ECOInvocation
parseECOOptions = go defaultECOOptions []
  where
    go o xs []
        | ecoAdapt o /= Nothing && (ecoReplay o /= Nothing || ecoCapture o /= Nothing || ecoBackendPlan o /= Nothing) =
            Left "-eco-adapt cannot be combined with replay or capture"
        | ecoBackendPlan o /= Nothing && ecoReplay o == Nothing =
            Left "-eco-backend-plan requires -eco-replay"
        | otherwise = validateInvocation o (reverse xs)
    go o xs (flag:value:rest)
        | flag == "-eco-graph" = once (ecoGraphDir o) $ go (o {ecoGraphDir = Just value}) xs rest
        | flag == "-eco-backend-plan" = once (ecoBackendPlan o) $ go (o {ecoBackendPlan = Just value}) xs rest
        | flag == "-eco-capture" = once (ecoCapture o) $ go (o {ecoCapture = Just value}) xs rest
        | flag == "-eco-replay" = once (ecoReplay o) $ go (o {ecoReplay = Just value}) xs rest
        | flag == "-eco-output" = once (ecoOutput o) $ go (o {ecoOutput = Just value}) xs rest
        | flag == "-eco-adapt" = once (ecoAdapt o) $ go (o {ecoAdapt = Just value}) xs rest
    go o xs (x:rest)
        | "-eco-" `isPrefixOf` x = Left ("unknown ECO option or missing argument: " ++ x)
        | otherwise = go o (x:xs) rest
    once Nothing x = x
    once (Just _) _ = Left "duplicate ECO option"

validateInvocation :: ECOOptions -> [String] -> Either String ECOInvocation
validateInvocation opts args = case ecoAdapt opts of
    Just capture
        | not (null args) -> Left "ECO adaptation uses captured flags; ordinary arguments are not allowed"
        | otherwise -> case ecoOutput opts of
            Nothing -> Left "-eco-adapt requires -eco-output DIR"
            Just output -> Right (AdaptInvocation capture output (ecoGraphDir opts))
    Nothing -> case ecoReplay opts of
        Just capture
            | not (null args) -> Left "ECO replay uses captured flags; ordinary arguments are not allowed"
            | ecoCapture opts /= Nothing -> Left "-eco-capture cannot be combined with -eco-replay"
            | otherwise -> case ecoOutput opts of
                Nothing -> Left "-eco-replay requires -eco-output DIR"
                Just output -> Right (ReplayInvocation capture output (ecoBackendPlan opts) (ecoGraphDir opts))
        Nothing
            | ecoOutput opts /= Nothing -> Left "-eco-output requires -eco-replay"
            | otherwise -> Right (CompileInvocation (ecoCapture opts) (ecoGraphDir opts) args)
