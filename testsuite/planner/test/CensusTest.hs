module CensusTest (runTests) where

import Prelude hiding (Word)
import BscTestsuite.Census
import BscTestsuite.Tcl
import Control.Exception (bracket)
import Control.Monad (forM_, unless)
import Data.Char (ord)
import Data.List (isInfixOf, sort)
import Numeric (showHex)
import System.Directory
  ( createDirectory, createDirectoryIfMissing, createFileLink, getTemporaryDirectory
  , removeFile, removePathForcibly )
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)

runTests :: IO ()
runTests = do
  lexicalTests
  censusTests
  differentialTests
  discoveryTests
  putStrLn "Census lexer, inert Tcl differential, and discovery tests passed."

check :: String -> Bool -> IO ()
check label ok = unless ok (ioError (userError ("CensusTest: " ++ label)))

parsed :: String -> Script
parsed input = case parseScript "fixture.exp" input of
  Left err -> error (show err)
  Right result -> result

values :: String -> [[Maybe String]]
values = map (map staticWord . commandWords) . scriptCommands . parsed

lexicalTests :: IO ()
lexicalTests = do
  check "braces, quotes, comments, and semicolons"
    (values "# ignored [exit]\nset a {one; [two] $three}; set b \"four five\"\n" ==
      [[Just "set", Just "a", Just "one; [two] $three"], [Just "set", Just "b", Just "four five"]])
  check "comment marker inside a command is an ordinary word"
    (values "set x #literal" == [[Just "set", Just "x", Just "#literal"]])
  check "escaped comment newline continues the comment"
    (values "# comment \\\n set hidden 1\nset visible 2" == [[Just "set", Just "visible", Just "2"]])
  check "continuation between words"
    (values "set \\\n  x \\\n\tvalue" == [[Just "set", Just "x", Just "value"]])
  check "continuation inside braces is substituted"
    (values "set x {a\\\n   b}" == [[Just "set", Just "x", Just "a b"]])
  check "escaped braces are preserved and do not affect nesting"
    (values "set x {a\\}b}" == [[Just "set", Just "x", Just "a\\}b"]])
  check "braces and quotes inside a bare word are ordinary characters"
    (values "set x a{b\"c" == [[Just "set", Just "x", Just "a{b\"c"]])
  check "bracket substitutions nest without consuming later words"
    (map (length . commandWords) (scriptCommands (parsed "set x [list [list a] b]")) == [3])
  check "braced variable names preserve spaces and brackets"
    (values "set x ${a [not-a-command]}" == [[Just "set", Just "x", Nothing]])
  check "array indices preserve whitespace and nested substitutions"
    (values "set x $a([list two words])" == [[Just "set", Just "x", Nothing]])
  check "escaped dollar is literal"
    (values "set x \\$foo" == [[Just "set", Just "x", Just "$foo"]])
  check "Unicode spaces remain inside Tcl words"
    (values "set x one\xa0\&two\x2003\&three" == [[Just "set", Just "x", Just "one\xa0\&two\x2003\&three"]])
  check "Tcl 8.6 non-BMP Unicode escapes preserve unconsumed suffixes"
    (values "capture \\U0001f642 \\U00110000 \\UFFFFFFFF" ==
      [[Just "capture", Just "\xfffd", Just "\xfffd\&0", Just "\xfffd\&FFF"]])
  check "argument expansion is explicitly dynamic"
    (values "call {*}{a b}" == [[Just "call", Nothing]])
  forM_ ["set x {", "set x \"", "set x [list a", "set x {a}b", "set x \"a\"b", "set x ${open", "set x $a(open"] $ \input ->
    check ("reject malformed Tcl: " ++ show input) (case parseScript "bad.exp" input of Left _ -> True; Right _ -> False)
  let commands = scriptCommands (parsed "\n  set x 1\n\tset y 2")
  check "positions use source lines and columns"
    (map (\c -> (sourceLine (commandPosition c), sourceColumn (commandPosition c))) commands == [(2,3),(3,2)])
  case parseListAt (SourcePos "list" 1 1 0) "#literal {a b} [not-executed];x" of
    Left err -> ioError (userError (show err))
    Right ws -> check "Tcl list words have no comment or substitution syntax"
      (map staticWord ws == map Just ["#literal", "a b", "[not-executed];x"])

censusTests :: IO ()
censusTests = do
  let report = censusText "nested.exp" $ unlines
        [ "set value [outer [inner]]"
        , "if {$value} {"
        , "  compile_pass Foo.bs"
        , "} elseif {0} {"
        , "  source never-open-this.exp"
        , "} else {"
        , "  exec never-run-this"
        , "}"
        , "proc helper {arg} { sim_pass Foo.bs }"
        , "set literal {not_a_script arg}"
        , "switch -- $value {first {first_call} default {second_call}}"
        , "catch $dynamic"
        ]
      sites = censusSites report
      constructs = map siteConstruct sites
  check "no lexical errors in nested fixture" (null (censusErrors report))
  forM_ ["set", "outer", "inner", "if", "compile_pass", "source", "exec", "proc", "sim_pass", "switch", "first_call", "second_call", "catch body"] $ \name ->
    check ("nested construct retained: " ++ name) (name `elem` constructs)
  check "ordinary brace values are not guessed to be scripts" ("not_a_script" `notElem` constructs)
  check "harness source line preserved" ([sourceLine (sitePosition s) | s <- sites, siteConstruct s == "compile_pass"] == [3])
  check "all sites are explicitly unlowered" (all (isInfixOf "not-yet-lowered" . siteReason) sites)
  check "census does not claim check counts" ("source-sites-not-tests" `isInfixOf` renderCensusJson report)
  let dynamic = censusText "dynamic.exp" "$command one"
  check "dynamic command retains exact syntax"
    (map siteConstruct (censusSites dynamic) == ["<dynamic-command: $command>"])
  let bad = censusText "bad.exp" "compile_pass {"
  check "parse failures are explicit and keep filename"
    (case censusErrors bad of [e] -> sourceFile (errorPosition e) == "bad.exp"; _ -> False)
  check "JSON escapes paths and syntax"
    ("quote\\\"\\u000a.exp" `isInfixOf` renderCensusJson (censusText "quote\"\n.exp" "set x 1"))
  let continuedSwitch = censusText "continued.exp" $ unlines
        ["switch -- value {", "  first {call", "    \\", "    continuation_call}", "}"]
      continuedSites = censusSites continuedSwitch
  check "switch continuation preprocessing is explicitly uninspected"
    (map siteConstruct continuedSites == ["switch", "switch cases"] && null (censusErrors continuedSwitch))
  check "switch continuation diagnostic identifies its exact source position"
    ([(sourceLine (sitePosition s), sourceColumn (sitePosition s))
      | s <- continuedSites, siteConstruct s == "switch cases"] == [(3,5)])

-- Only these fixed, inert fixtures are passed to Tcl. The repository corpus is
-- never sourced or evaluated, and capture only prints its arguments as hex.
differentialTests :: IO ()
differentialTests = do
  (versionExit, version, versionErr) <- readProcessWithExitCode "tclsh" [] "puts [info tclversion]\n"
  check "differential oracle must be supported Tcl 8.6"
    (versionExit == ExitSuccess && version == "8.6\n" && null versionErr)
  forM_ fixtures $ \fixture -> do
    let scriptText = "proc capture {args} {foreach arg $args {puts [binary encode hex [encoding convertto utf-8 $arg]]}}\n" ++
          "uplevel #0 [encoding convertfrom utf-8 [binary decode hex " ++ hex fixture ++ "]]\n"
        expected = case values fixture of
          [Just "capture":args] -> map (maybe (error "dynamic differential fixture") hex) args
          _ -> error "invalid differential fixture"
    compareTcl "script word splitting" fixture scriptText expected
  forM_ listFixtures $ \fixture -> do
    -- Decode the fixture as inert data: no part of its text is sourced or
    -- substituted as Tcl program text, even when it contains [exit].
    let scriptText = "set input [encoding convertfrom utf-8 [binary decode hex " ++ hex fixture ++ "]]\n" ++
          "foreach arg $input {puts [binary encode hex [encoding convertto utf-8 $arg]]}\n"
        expected = case parseListAt (SourcePos "list-fixture" 1 1 0) fixture of
          Left err -> error (show err)
          Right ws -> map (maybe (error "dynamic list differential fixture") hex . staticWord) ws
    compareTcl "list word splitting" fixture scriptText expected
  where
    compareTcl label fixture scriptText expected = do
      (exitCode, output, err) <- readProcessWithExitCode "tclsh" [] scriptText
      check ("tclsh fixture exits cleanly: " ++ show fixture) (exitCode == ExitSuccess && null err)
      check (label ++ " agrees with tclsh: " ++ show fixture) (lines output == expected)
    fixtures =
      [ "capture one {two words} \"three words\" {} \"\""
      , "capture foo\\ bar \\n \\101 \\x41 \\u0042"
      , "capture {literal $name [exit] ; #} \\[literal\\] \\$name"
      , "capture {a\\}b} {nested {braces}}"
      , "capture one \\\n  two {three\\\n  four}"
      , "capture one\\\n  two"
      , "capture \\377 \\400 \\x414 \\u00421"
      , "capture one\xa0\&two one\x2003\&two"
      , "capture one\ttwo\vthree\ffour\rfive"
      , "capture \\U0001f642 \\U00110000 \\UFFFFFFFF"
      ]
    listFixtures =
      [ "a\\\nb"
      , "{a\\\n b}"
      , "\"a\\\n b\""
      , "\\\n"
      , "one\xa0\&two one\x2003\&two"
      , "one\ttwo\vthree\ffour\rfive\nsix"
      , "{literal $name [exit] ; #}"
      , "\\U0001f642 \\U00110000 \\UFFFFFFFF"
      ]
    hex = concatMap (concatMap byteHex . utf8 . ord)
    byteHex b = let h = showHex b "" in replicate (2 - length h) '0' ++ h
    utf8 n
      | n < 0x80 = [n]
      | n < 0x800 = [0xc0 + n `div` 64, 0x80 + n `mod` 64]
      | n < 0x10000 = [0xe0 + n `div` 4096, 0x80 + (n `div` 64) `mod` 64, 0x80 + n `mod` 64]
      | otherwise = [0xf0 + n `div` 262144, 0x80 + (n `div` 4096) `mod` 64, 0x80 + (n `div` 64) `mod` 64, 0x80 + n `mod` 64]

discoveryTests :: IO ()
discoveryTests = withTemporaryDirectory $ \root -> do
  let ordinary = root </> "bsc.fixture"
      long = root </> "bsc.long_tests"
      config = root </> "config"
  mapM_ (createDirectoryIfMissing True) [ordinary, long, config]
  writeFile (ordinary </> "ordinary.exp") "compile_pass Foo.bs\n"
  writeFile (long </> "active.exp.golden") "active_call\n"
  createFileLink "active.exp.golden" (long </> "active.exp")
  writeFile (long </> "inactive.exp.golden") "inactive_call\n"
  writeFile (config </> "unix.exp") "must_not_inspect\n"
  active <- censusTree root
  check "active corpus follows .exp symlinks and excludes config"
    (sort (map siteConstruct (censusSites active)) == ["active_call", "compile_pass"])
  check "inactive long template remains visible without being counted as active"
    (censusInactiveTemplates active == [long </> "inactive.exp.golden"])
  named <- censusTree ordinary
  trailing <- censusTree (ordinary ++ "/")
  check "named corpus directory is invariant under a trailing slash"
    (named == trailing && length (censusFiles trailing) == 1)
  allSources <- censusTreeIncludingTemplates root
  check "template inclusion is explicit and does not double-count active symlinks"
    (sort (map siteConstruct (censusSites allSources)) == ["active_call", "compile_pass", "inactive_call"])

withTemporaryDirectory :: (FilePath -> IO a) -> IO a
withTemporaryDirectory action = bracket create removePathForcibly action
  where
    create = do
      temporary <- getTemporaryDirectory
      (path, handle) <- openTempFile temporary "bsc-planner-census"
      hClose handle
      removeFile path
      createDirectory path
      pure path
