module Language.PipeScript.Interpreter.Task
  ( createTaskSet,
    isEmptyTaskSet,
    takeTask,
    runTasksOneByOne,
  )
where

import Control.Monad (void, when)
import Data.Foldable (foldl')
import Data.HashSet
  ( HashSet,
    difference,
    empty,
    fromList,
    member,
    union,
  )
import Data.List (delete, find)
import Data.Time.Clock (UTCTime)
import Language.PipeScript.Interpreter.Context
import Language.PipeScript.Interpreter.Eval
import Path
import Path.IO (doesFileExist, getModificationTime)
import System.ProgressBar


data TaskSet = TaskSet [Task] (HashSet (Path Abs File)) (HashSet (Path Abs File))

createTaskSet :: [Task] -> TaskSet
createTaskSet tasks = TaskSet tasks (fromList (tasks >>= outputFiles)) empty

isEmptyTaskSet :: TaskSet -> Bool
isEmptyTaskSet (TaskSet [] _ _) = True
isEmptyTaskSet _ = False

takeTask :: Int -> TaskSet -> IO ([Task], TaskSet)
takeTask 0 t = pure ([], t)
takeTask n (TaskSet tasks plannedOutput alreadyOutput) =
  case find inputsNotExistsInPlannedOutput tasks of
    Nothing -> pure ([], TaskSet tasks plannedOutput alreadyOutput)
    Just task -> do
      task' <- testTask task
      (task'', TaskSet remainTasks plannedOutput' alreadyOutput') <-
            takeTask (n - 1) $ TaskSet (Data.List.delete task tasks) plannedOutput alreadyOutput
      let outTasks = task' : task''
          alreadyOutput'' = 
            if forceDirty task' 
              then alreadyOutput' `union` fromList (outputFiles task)
              else alreadyOutput'
          plannedOutput'' = plannedOutput' `difference` fromList (outputFiles task)
       in pure (outTasks, TaskSet remainTasks plannedOutput'' alreadyOutput'')
  where
    inputsNotExistsInPlannedOutput task =
      not (any (`member` plannedOutput) (inputFiles task))
    inputIsInAlreadyOutput = pure . not . any (`member` alreadyOutput) . inputFiles
    tests = wrapTest <$> [inputIsInAlreadyOutput, outputFileExistsTest, timeTest]
    testTask x = foldl' (>>=) (pure x) tests

taskCount :: TaskSet -> Int
taskCount (TaskSet tasks _ _) = length tasks

outputFileExistsTest :: Task -> IO Bool
outputFileExistsTest x = and <$> mapM doesFileExist (outputFiles x)

timeTest :: Task -> IO Bool
timeTest task =
  case (inputFiles task, outputFiles task) of
    ([], []) -> return True
    ([], _) -> return True
    (_, []) -> return True
    (inputs, outputs) -> do
      outTime <- oldestOutputFileModifyTime
      inTime <- newestInputFileModifyTime
      return (inTime < outTime)
      where
        oldestOutputFileModifyTime =
          minimum <$> mapM getModificationTime outputs
        newestInputFileModifyTime =
          maximum <$> mapM getModificationTime inputs

wrapTest :: (Task -> IO Bool) -> Task -> IO Task
wrapTest test task =
  if forceDirty task
    then pure task
    else do
      result <- test task
      if result
        then return task
        else return task {forceDirty = True}

runTask :: Task -> IO ()
runTask task = 
  let interpreter = runTopLevelByName (operationName task) $ arguments task in
  when (forceDirty task) $
    void $ run interpreter $ (context task) {isPreRun = False}


runTasksOneByOne :: [Task] -> IO ()
runTasksOneByOne tasks = do
  let pbarStyle = defStyle { stylePostfix = exact, styleTodo = ' ' }
      allTaskCount = length tasks
  pbar <- newProgressBar pbarStyle 24 $ Progress 0 allTaskCount ()
  let taskSet = createTaskSet tasks
      runTasks taskSet = do
        if isEmptyTaskSet taskSet
          then updateProgress pbar $ 
                \x -> x { progressDone = allTaskCount }
          else do
            (task, taskSet') <- takeTask 1 taskSet
            mapM_ runTask task
            when (any forceDirty task) $
              updateProgress pbar $ 
                \x -> x { progressDone = allTaskCount - taskCount taskSet' }
            runTasks taskSet'
  runTasks taskSet
