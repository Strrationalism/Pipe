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
import Path (parseRelFile)
import Path.IO (doesFileExist)
import System.Environment (getArgs)
import qualified System.Info

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
  parseStep defaultArgument args
  where
    parseStep prev = \case
      [] -> prev
      "--for" : os : more -> parseStep (prev {currentPlatform = os}) more
      "--to" : os : more -> parseStep (prev {targetPlatform = os}) more
      "--verbose" : more -> parseStep (prev {verbose = True}) more
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
          putStrLn ("Error: " ++ show startupScript ++ " not exists.")
            >> putStrLn ""
            >> help
        else do
          scripts <- parsePipeScriptWithIncludes startupScript
          let (errs, scrs) = partitionEithers scripts
          if null errs
            then
              let for = currentPlatform args
                  to = targetPlatform args
                  nubScrs = nubBy (\s1 s2 -> scriptPath s1 == scriptPath s2) scrs
                  scrsCurPlat = onlyThisPlatform for to nubScrs
                  pipeCommandLine = actionNameAndArgs args
                  actionToStart = case pipeCommandLine of
                    [] -> "build"
                    a : _ -> a
                  actionArguments = case pipeCommandLine of
                    [] -> []
                    _ : a -> a
               in print scrsCurPlat
               -- Start 'actionToStart' action with 'actionArguments'
            else do
              let printError [] = return ()
                  printError (a : ls) = print a >> putStrLn "" >> printError ls
              printError errs