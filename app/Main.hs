{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Either (partitionEithers)
import Data.List (nubBy)
import Language.PipeScript (AST)
import Language.PipeScript.Parser
  ( onlyThisPlatform,
    parsePipeScriptWithIncludes,
    scriptPath
  )
import Language.PipeScript.Interpreter.Context hiding (verbose)
import Path (parseRelFile, toFilePath)
import Path.IO (doesFileExist)
import System.Environment (getArgs)
import qualified System.Info
import System.Exit (exitWith, ExitCode (ExitFailure))
import Language.PipeScript.Interpreter.Eval
import Language.PipeScript.Interpreter.Context (run)
import Control.Monad (void)
import Language.PipeScript.Interpreter.PipeLibrary (loadLibrary)
import Language.PipeScript.Interpreter.Task
import System.Console.Pretty
import Control.Exception


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
  "darwin" -> "macos"
  x -> x

defaultArgument :: Argument
defaultArgument =
  Argument
    { currentPlatform = os,
      targetPlatform = os,
      actionNameAndArgs = [],
      verbose = True
    }

parseArgs :: [String] -> Argument
parseArgs ["--help"] = Help
parseArgs ["-h"] = Help
parseArgs args =
  parseStep defaultArgument args
  where
    parseStep prev = \case
      [] -> prev
      "--for" : os : more -> parseStep (prev {currentPlatform = os}) more
      "--to" : os : more -> parseStep (prev {targetPlatform = os}) more
      p : more | p == "-p" || p == "--parallel" -> parseStep (prev {verbose = False}) more
      x : more -> parseStep (prev {actionNameAndArgs = actionNameAndArgs prev ++ [x]}) more

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
            "    --help (or -h)      Show help information.",
            "    --for <os>          Set current platform. (default: " ++ os ++ ")",
            "    --to <os>           Set target platform. (default: " ++ os ++ ")",
            "    --parallel (or -p)  Build in parallel mode and show progress.",
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
          putStrLn ("Error: " ++ show startupScript ++ " not exists.")
            >> putStrLn ""
            >> help
            >> exitWith (ExitFailure 2)
        else do
          scripts <- parsePipeScriptWithIncludes $ toFilePath startupScript
          let (errs, scrs) = partitionEithers scripts
          if null errs
            then
              let for = currentPlatform args
                  to = targetPlatform args
                  isSamePath s1 s2 = scriptPath s1 == scriptPath s2
                  nubScrs = nubBy isSamePath scrs
                  scrsCurPlat = onlyThisPlatform for to nubScrs
                  pipeCommandLine = actionNameAndArgs args
                  actionToStart = case pipeCommandLine of
                    [] -> "build"
                    a : _ -> a
                  actionArguments = case pipeCommandLine of
                    [] -> []
                    _ : a -> a
                  taskRunner = if verbose args then runTasksOneByOne else runTasksParallel
                  context = loadLibrary $ createContext (verbose args) scrsCurPlat taskRunner
                  interpreter = runAction True actionToStart $ fmap ValStr actionArguments
               in catch (void $ run interpreter context) (\e -> do
                    pretty <- supportsPretty
                    let putStrLnStyled = if pretty then putStrLn . color Red else putStrLn
                    putStrLnStyled $ show (e :: SomeException))
            else
              let printError [] = return ()
                  printError (a : ls) =
                    print a >> putStrLn "" >> printError ls
              in printError errs >> exitWith (ExitFailure 3)
