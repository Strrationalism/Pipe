module Main where

import Language.PipeScript (AST)
import System.Environment (getArgs)
import qualified System.Info
import Path
import Path.IO
import Language.PipeScript.Parser
import Data.List
import Data.Either (partitionEithers)

data Argument
  = Argument
      { currentPlatform :: String,
        targetPlatform :: String,
        actionNameAndArgs :: [String],
        verbose :: Bool
      }
  | Help

os :: String
os = case System.Info.os of
  "mingw32" -> "windows"
  x -> x

defaultArgument :: Argument
defaultArgument =
  Argument
    { currentPlatform = os,
      targetPlatform = os,
      actionNameAndArgs = [],
      verbose = False
    }

parseArgs :: [String] -> Argument
parseArgs ["--help"] = Help
parseArgs args =
  setDefaultActionName $ parseStep args defaultArgument
  where
    parseStep [] prev = prev
    parseStep ("--for" : os : more) prev = parseStep more $ prev {currentPlatform = os}
    parseStep ("--to" : os : more) prev = parseStep more $ prev {targetPlatform = os}
    parseStep ("--verbose" : more) prev = parseStep more $ prev {verbose = True}
    parseStep (x : more) prev = parseStep more $ prev {actionNameAndArgs = actionNameAndArgs prev ++ [x]}
    setDefaultActionName prev =
      case prev of
        Argument {actionNameAndArgs = []} -> prev {actionNameAndArgs = ["build"]}
        x -> x

help :: IO ()
help =
  sequence_ $
    putStrLn
      <$> [ "Pipe Build Automation Tool",
            "by Strrationalism Studio 2021",
            "",
            "Usage:",
            "    pipe [--help] [--for <os>] [--to <os>] [--verbose] [action [action arguments ...]]",
            "",
            "Flags:",
            "    --help      Show help information.",
            "    --for <os>  Set current platform. (default: " ++ os ++ ")",
            "    --to <os>   Set target platform. (default: " ++ os ++ ")",
            "    --verbose   Show build logs.",
            ""
          ]

main :: IO ()
main = do
  args <- parseArgs <$> getArgs
  case args of
    Help -> help
    Argument {} -> do
      startupScript <- parseRelFile "./build.pipe"
      scriptExists <- doesFileExist startupScript
      if not scriptExists
        then
          putStrLn ("Error: \'" ++ show startupScript ++ "\' not exists.")
            >> putStrLn ""
            >> help
        else do
          scripts <- parsePipeScriptWithIncludes startupScript
          let (errs, scrs) = partitionEithers scripts
          if null errs then
            let for = currentPlatform args
                to = targetPlatform args
                nubScrs = nubBy (\s1 s2 -> scriptPath s1 == scriptPath s2) scrs
                scrsCurPlat = onlyThisPlatform for to nubScrs in
            print scrsCurPlat
          else do
            let printError [] = return ()
                printError (a : ls) = print a >> putStrLn "" >> printError ls
            printError errs