module Language.PipeScript.Interpreter.Task
  ( createTaskSet,
    isEmptyTaskSet,
    takeTask,
    runTasksOneByOne,
    runTasksParallel,
  )
where

import Control.Concurrent
  ( MVar,
    forkIO,
    forkOS,
    getChanContents,
    newChan,
    newEmptyMVar,
    putMVar,
    takeMVar,
    writeChan,
  )
import Control.Concurrent.Chan (Chan)
import Control.Exception (Exception, throw)
import Control.Exception.Base (SomeException, try)
import Control.Monad (unless, void, when)
import Data.Data (Typeable)
import Data.Either (partitionEithers)
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
import Debug.Trace (trace)
import GHC.Conc (getNumProcessors)
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

takeTask :: TaskSet -> IO ([Task], TaskSet)
takeTask (TaskSet tasks plannedOutput alreadyOutput) =
  case find inputsNotExistsInPlannedOutput tasks of
    Nothing -> pure ([], TaskSet tasks plannedOutput alreadyOutput)
    Just task -> do
      task' <- testTask task
      if dirty task'
        then do
          (task'', TaskSet remainTasks plannedOutput' alreadyOutput') <-
            takeTask $ TaskSet (Data.List.delete task tasks) plannedOutput alreadyOutput
          let outTasks = task' : task''
              alreadyOutput'' = alreadyOutput' `union` fromList (outputFiles task')
              plannedOutput'' = plannedOutput' `difference` fromList (outputFiles task')
              nextTaskSet = TaskSet remainTasks plannedOutput'' alreadyOutput''
          pure (outTasks, nextTaskSet)
        else do
          let plannedOutput' = plannedOutput `difference` fromList (outputFiles task')
              nextTaskSet = TaskSet (Data.List.delete task tasks) plannedOutput' alreadyOutput
          takeTask nextTaskSet
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
  if dirty task
    then pure task
    else do
      result <- test task
      if result
        then return task
        else return task {dirty = True}

runTask :: Task -> IO ()
runTask task =
  let interpreter = runTopLevelByName (operationName task) $ arguments task
   in when (dirty task) $
        void $ run interpreter $ (context task) {isPreRun = False}

runTasksOneByOne :: [Task] -> IO ()
runTasksOneByOne tasks = do
  let taskSet = createTaskSet tasks
      runTasks taskSet = do
        unless (isEmptyTaskSet taskSet) $ do
          (task, taskSet') <- takeTask taskSet
          mapM_ runTask task
          runTasks taskSet'
  runTasks taskSet

worker :: MVar (Either () Task) -> Chan (Either SomeException ()) -> IO ()
worker taskVar resultOut = do
  trace "Worker is waiting for a task" $ pure ()
  task <- takeMVar taskVar
  case task of
    Left () -> trace "Worker get a ()" $ return ()
    Right task -> do
      trace "Worker get a task" $ pure ()
      result <- try $ runTask task
      trace ("Result is " ++ show result) $ pure ()
      writeChan resultOut result
      worker taskVar resultOut

newtype MultiExceptions = MultiExceptions [SomeException] deriving (Typeable)

instance Show MultiExceptions where
  show (MultiExceptions xs) = foldl' (\a b -> a ++ "\n" ++ "\n" ++ b) "" $ map show xs

instance Exception MultiExceptions

runTasksParallel :: [Task] -> IO ()
runTasksParallel tasks = do
  let pbarStyle = defStyle {stylePostfix = exact, styleTodo = ' ', styleOnComplete = Clear}
      allTaskCount = length tasks
      taskSet = createTaskSet tasks
  pbar <- newProgressBar pbarStyle 24 $ Progress 0 allTaskCount ()
  cores <- getNumProcessors
  scheduleTasks <- newEmptyMVar
  resultOut <- newChan
  workers <- sequence $ forkIO (worker scheduleTasks resultOut) <$ [0 .. cores - 1]

  let runTasks [] taskSet = do
        if isEmptyTaskSet taskSet
          then do
            updateProgress pbar $
              \x -> x {progressDone = allTaskCount}
            mapM_ (const $ putMVar scheduleTasks $ Left ()) workers
          else do
            trace "Master geting errors" $ pure ()
            -- 注意：Blocked是由于Channel被阻塞导致
            --errs <- fst . partitionEithers <$> getChanContents resultOut
            --trace ("Master got " ++ show errs) $ pure ()
            --case errs of
              --[] -> takeTask taskSet >>= uncurry runTasks
              --_ -> do
                --mapM_ (const $ putMVar scheduleTasks $ Left ()) workers
                --throw $ MultiExceptions errs
      runTasks (firstTask : restTasks) taskSet = do
        trace "Mater put a task" $ pure ()
        putMVar scheduleTasks $ Right firstTask
        incProgress pbar 1
        runTasks restTasks taskSet

  takeTask taskSet >>= uncurry runTasks