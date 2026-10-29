-- ECO command-line parsing stays separate from ordinary flag decoding.
-- Parse only ordinary Strings so replay can restore the intern table first.
module ECO.Options (ECOOptions(..), ECOInvocation(..), defaultECOOptions,
                   parseECOOptions, invocationOptions, ecoEnabled) where

import Warmup ()

import Data.List (isPrefixOf)

data ECOOptions = ECOOptions
    { ecoCapture :: Maybe FilePath
    , ecoReplay :: Maybe FilePath
    , ecoOutput :: Maybe FilePath
    , ecoAdapt :: Maybe FilePath
    } deriving (Eq, Show)

defaultECOOptions :: ECOOptions
defaultECOOptions = ECOOptions Nothing Nothing Nothing Nothing

ecoEnabled :: ECOOptions -> Bool
ecoEnabled o = o /= defaultECOOptions

-- CLI modes carry their required paths directly. This transient value contains
-- only ordinary Strings; constructing it cannot touch the compiler interner.
-- ECOOptions remains the invocation-only runtime projection stored in Flags.
data ECOInvocation
    = CompileInvocation (Maybe FilePath) [String]
      -- capture directory, ordinary compiler arguments
    | ReplayInvocation FilePath FilePath
      -- capture, output directory
    | AdaptInvocation FilePath FilePath
      -- capture, output directory
    deriving (Eq, Show)

invocationOptions :: ECOInvocation -> ECOOptions
invocationOptions invocation = case invocation of
    CompileInvocation capture _ -> defaultECOOptions
        { ecoCapture = capture }
    ReplayInvocation capture output -> defaultECOOptions
        { ecoReplay = Just capture, ecoOutput = Just output }
    AdaptInvocation capture output -> defaultECOOptions
        { ecoAdapt = Just capture, ecoOutput = Just output }

parseECOOptions :: [String] -> Either String ECOInvocation
parseECOOptions = go defaultECOOptions []
  where
    go o xs []
        | ecoAdapt o /= Nothing && (ecoReplay o /= Nothing || ecoCapture o /= Nothing) =
            Left "-eco-adapt cannot be combined with replay or capture"
        | otherwise = validateInvocation o (reverse xs)
    go o xs (flag:value:rest)
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
            Just output -> Right (AdaptInvocation capture output)
    Nothing -> case ecoReplay opts of
        Just capture
            | not (null args) -> Left "ECO replay uses captured flags; ordinary arguments are not allowed"
            | ecoCapture opts /= Nothing -> Left "-eco-capture cannot be combined with -eco-replay"
            | otherwise -> case ecoOutput opts of
                Nothing -> Left "-eco-replay requires -eco-output DIR"
                Just output -> Right (ReplayInvocation capture output)
        Nothing
            | ecoOutput opts /= Nothing -> Left "-eco-output requires -eco-replay"
            | otherwise -> Right (CompileInvocation (ecoCapture opts) args)
