module Language.PipeScript.Interpreter.Eval (runAction, evalExpr, evalError, runTopLevelByName, EvalException) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class
import Data.Foldable
import Data.HashMap.Strict (member, (!?))
import qualified Data.HashMap.Strict
import qualified Data.HashSet
import Data.Maybe
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import GHC.IO.Handle (hClose)
import Language.PipeScript
import Language.PipeScript.Interpreter.Context
import Language.PipeScript.Parser
import Path (toFilePath, parseAbsFile)
import System.Exit (exitFailure)
import System.IO
import System.Process hiding (runCommand)
import System.Console.Pretty
import Debug.Trace (trace)
import Data.Bifunctor (second)
import Control.Exception (throw, Exception)
import Type.Reflection (Typeable)


newtype EvalException = EvalException String deriving (Typeable)

instance Show EvalException where
  show (EvalException msg) = msg

instance Exception EvalException


evalError :: String -> Interpreter a
evalError x = do
  script <- curScript <$> get
  topLevel <- curTopLevel <$> get
  throw $ EvalException $ unlines
    [ "In " ++ show (scriptPath script) ++ " (" ++ show topLevel ++ "):",
      "  " ++ x
    ]


loadStr :: String -> Interpreter Value
loadStr x = do
  (isAbs, str) <- eval x Nothing
  if isAbs
    then ValAbsPath <$> parseAbsFile str
    else pure $ ValStr str
  where
    eval [] Nothing = pure (False, "")
    eval [] (Just v) = evalError $ "Invalid string with unclosed variable \"" ++ v ++ "\"."
    eval ('%' : '%' : ls) varState = second ('%' :) <$> eval ls varState
    eval ('%' : ls) Nothing = eval ls $ Just ""
    eval (a : ls) Nothing = second (a :) <$> eval ls Nothing
    eval ('%' : ls) (Just var) = do
      val <- getVariable $ Variable $ Identifier var
      (isAbs, next) <- eval ls Nothing
      let show' (ValStr x) = (isAbs, x)
          show' (ValAbsPath x) = (True, toFilePath x)
          show' x = (isAbs, show x)
          (isAbs', x) = show' val
      pure (isAbs' || isAbs, x ++ next)
    eval (a : ls) (Just var) = eval ls $ Just $ var ++ [a]

runCommand :: FilePath -> [String] -> Interpreter Value
runCommand command args = do
  isVerbose <- verbose <$> get
  workdir <- scriptDir . curScript <$> get
  e <- getVariableEnvs

  liftIO $ when isVerbose $ do
      pretty <- supportsPretty
      if pretty
      then do
        putStr $ color Cyan $ style Faint workdir
        putStr " "
        putStr $ color Yellow $ style Bold command
        putStr " "
        mapM_ (\x -> putStr (color Yellow x) >> putStr " ") args
        putStrLn ""
      else do
        putStr workdir
        putStr " "
        putStr command
        putStr " "
        mapM_ (\x -> putStr x >> putStr " ") args
        putStrLn ""

  let outStream = if isVerbose then Inherit else NoStream
  (_, _, _, process) <- liftIO $ createProcess $
    CreateProcess
      { cmdspec = RawCommand command args,
        cwd = Just workdir,
        env = Just $ fmap (\(Variable (Identifier var), val) -> (var, show val)) e,
        std_in = Inherit,
        std_out = outStream,
        std_err = outStream,
        close_fds = False,
        create_group = False,
        delegate_ctlc = False,
        detach_console = False,
        create_new_console = False,
        new_session = False,
        use_process_jobs = True ,
        child_group = Nothing,
        child_user = Nothing
      }
  exitCode <- liftIO $ waitForProcess process

  case exitCode of
    ExitSuccess -> return ()
    ExitFailure x -> do
      evalError $
        unlines
          [ command
            ++ " with arguments "
            ++ show args
            ++ " failed with exitcode "
            ++ show x
            ++ ", run pipe with --verbose for more information.",
            ""
          ]
  when isVerbose $ liftIO $ putStrLn ""
  return ValUnit

evalApplyExpr :: Expression -> [Expression] -> Interpreter Value
evalApplyExpr (IdentifierExpr (Identifier "set")) [VariableExpr var, expr] = do
  case var of Variable (Identifier "inputs") -> evalError "%inputs% is a read-only variable."
              Variable (Identifier "outputs") -> evalError "%outputs% is a read-only variable."
              Variable (Identifier "cd") -> evalError "%cd% is a read-only variable."
              Variable _ -> evalExpr expr >>= setVariable var >> return ValUnit
evalApplyExpr (IdentifierExpr (Identifier "set")) _ =
  evalError "Set function must has a variable argument and an expression argument."
evalApplyExpr (IdentifierExpr (Identifier i)) args = do
  context <- get
  let args' = mapM evalExpr args
  case Data.HashMap.Strict.lookup (Identifier i) $ funcs context of
    Just f -> args' >>= f
    Nothing -> do
      let topLevel = curTopLevel context
      topLevelsToCall <- getTopLevels i
      if not $ null topLevelsToCall
        then case topLevel of
          Include _ -> evalError "Here is a include toplevel?!"
          ActionDefination _ ->
            args' >>= runTopLevels i topLevelsToCall >> return ValUnit
          TaskDefination _ -> do
            let c = isOperation topLevelsToCall
                c' = isTask topLevelsToCall
            if c || c'
              then args' >>= runTopLevels i topLevelsToCall >> return ValUnit
              else evalError $ "Can not call " ++ i ++ " from a task."
          OperationDefination _ _ -> do
            let c = isOperation topLevelsToCall
            if c
              then args' >>= runTopLevels i topLevelsToCall >> return ValUnit
              else evalError $ "Can not call " ++ i ++ " from a operation."
        else evalExpr (ApplyExpr (ConstantExpr $ ConstStr i) args)
evalApplyExpr (ConstantExpr (ConstStr file)) args = do
  cmd <- value2Str <$> loadStr file
  args <- expandLists <$> mapM evalExpr args
  runCommand file $ fmap value2Str args
  where expandLists [] = []
        expandLists (ValList ls : next) = expandLists ls ++ expandLists next
        expandLists (a : next) = a : expandLists next
evalApplyExpr left rights = do
  left' <- evalExpr left
  case left' of
    ValStr str -> evalExpr $ ApplyExpr (IdentifierExpr $ Identifier str) rights
    x -> evalError $ "Can not apply to \"" ++ show x ++ "\"."

evalExpr :: Expression -> Interpreter Value
evalExpr (IdentifierExpr (Identifier name)) = return $ ValSymbol name
evalExpr (VariableExpr var) = getVariable var
evalExpr (ConstantExpr (ConstStr str)) = loadStr str
evalExpr (ConstantExpr c) = return $ valueFromConstant c
evalExpr (ListExpr ls) = ValList <$> mapM evalExpr ls
evalExpr (ApplyExpr left rights) = evalApplyExpr left rights


evalStatements :: [Statement] -> Interpreter ()
evalStatements = foldr' ((>>) . evalStatement) (return ())

evalStatement :: Statement -> Interpreter ()
evalStatement stat = do
  eval stat
  where
    eval (ExprStat (IdentifierExpr x)) =
      eval $ ExprStat $ ApplyExpr (IdentifierExpr x) []
    eval (ExprStat expr) = void $ evalExpr expr
    eval (IfStat [] defaultBranch) = evalStatements defaultBranch
    eval (IfStat ((cond, block) : next) defBranch) = do
      condVal <- evalExpr cond
      case condVal of
        ValBool True -> evalStatements block
        ValBool False -> eval $ IfStat next defBranch
        _ -> evalError "If statement condition expression must return a bool value."
    eval (ForEachLoop loopVar lsExpr block) = do
      ls <- evalExpr lsExpr
      case ls of
        ValList ls' -> do
          forM_ ls' $ \val -> do
            setVariable loopVar val
            evalStatements block
        _ -> evalError "ForEachLoop loop variable must be a list."

evalTopLevel' :: [Value] -> (Script, TopLevel) -> Interpreter ()
evalTopLevel' args (scr, tl) = evalTopLevel scr tl args

evalTopLevel :: Script -> TopLevel -> [Value] -> Interpreter ()
evalTopLevel script topLevel arguments = do
  isVerbose <- verbose <$> get
  liftIO $ when isVerbose $ do
    pretty <- supportsPretty
    let args' = foldl' (\a b -> a ++ " " ++ b) "" $ fmap show arguments
    let printStyle = if pretty then color Green . style Bold else id
    putStrLn $ printStyle $ "- " ++ show topLevel ++ args'

  prevScript <- curScript <$> get
  prevTopLevel <- curTopLevel <$> get
  modify $ \c -> c {curTopLevel = topLevel, curScript = script}
  eval topLevel
  modify $ \c -> c {curTopLevel = prevTopLevel, curScript = prevScript}
  when isVerbose $ liftIO $ putStrLn ""
  where
    eval (Include _) = return ()
    eval (ActionDefination b) = variableScope $ evalBlock b
    eval (TaskDefination b) = variableScope $ do
      prevTask <- curTask <$> get
      modify $ \c -> c {curTask = Just $ createTask (show $ name b) arguments}
      evalBlock b
      task <- curTask <$> get
      taskContext <- get
      modify $ \c -> c {curTask = prevTask}
      case task of
        Nothing -> return ()
        Just t -> modify $ \c -> c {tasks = t {context = taskContext} : tasks c}
    eval (OperationDefination BeforeAction b) = evalBlock b
    eval (OperationDefination AfterAction b) = evalBlock b
    eval (OperationDefination BeforeAll b) = evalBlock b
    eval (OperationDefination AfterAll b) = evalBlock b
    eval (OperationDefination t b) = variableScope $ evalBlock b
    evalBlock opBlock = do
      let params = parameters opBlock
      if length params == length arguments
        then do
          setVariables $ zip params arguments
          vars <- getVariables
          evalStatements $ block opBlock
        else
          evalError $
            "Can not match parameters " ++ show params ++ " with arguments " ++ show arguments ++ "."

isTask :: [(Script, TopLevel)] -> Bool
isTask x =
  not (isAction x) && isTask' x
  where
    isTask' [] = False
    isTask' ((_, TaskDefination _) : _) = True
    isTask' (_ : next) = isTask' next

isOperation :: [(Script, TopLevel)] -> Bool
isOperation x = not (isAction x) && not (isTask x)

isAction :: [(Script, TopLevel)] -> Bool
isAction [] = False
isAction ((_, ActionDefination _) : _) = True
isAction (_ : next) = isAction next

getTopLevels :: String -> Interpreter [(Script, TopLevel)]
getTopLevels name = do
  tls <- topLevels <$> get
  case tls !? name of
    Nothing -> return []
    Just x -> return x

runTopLevels :: String -> [(Script, TopLevel)] -> [Value] -> Interpreter ()
runTopLevels name tls args
  | isTask tls = do
    preRun <- isPreRun <$> get
    if preRun
      then run $ takeTasks tls
      else run $ takeOps tls
  | isAction tls = runAction' False name tls args
  | otherwise = run tls
  where
    run = mapM_ (evalTopLevel' args)
    isTaskBlock (_, TaskDefination _) = True
    isTaskBlock _ = False
    takeTasks = filter isTaskBlock
    takeOps = filter $ not . isTaskBlock

runTopLevelByName :: String -> [Value] -> Interpreter ()
runTopLevelByName x args = getTopLevels x >>= \x' -> runTopLevels x x' args

runAction' :: Bool -> String -> [(Script, TopLevel)] -> [Value] -> Interpreter ()
runAction' topRun name tls args = do
  allTopLevels <- concat . toList . topLevels <$> get
  let beforeAll = filter isBeforeAllBlock allTopLevels
      afterAll = filter isAfterAllBlock allTopLevels
      befores = takeBefore tls
      actions = takeAction tls
      afters = takeAfter tls

  if topRun
  then executeTopLevels beforeAll
  else pure ()

  executeTopLevels befores
  executeTopLevels actions

  t <- fmap tasks get
  runner <- fmap taskRunner get
  liftIO $ runner t

  executeTopLevels afters

  if topRun
  then executeTopLevels afterAll
  else pure ()

  where
    executeTopLevels = mapM_ executeTopLevel
    executeTopLevel = evalTopLevel' args
    isBeforeAllBlock (_, OperationDefination BeforeAll _) = True
    isBeforeAllBlock _ = False
    isAfterAllBlock (_, OperationDefination AfterAll _) = True
    isAfterAllBlock _ = False
    isBeforeBlock (_, OperationDefination BeforeAction _) = True
    isBeforeBlock _ = False
    takeBefore = filter isBeforeBlock
    isActionBlock (_, ActionDefination _) = True
    isActionBlock _ = False
    takeAction = filter isActionBlock
    isAfterBlock (_, OperationDefination AfterAction _) = True
    isAfterBlock _ = False
    takeAfter = filter isAfterBlock

runAction :: Bool -> String -> [Value] -> Interpreter ()
runAction topRun name args = getTopLevels name >>= \tls -> runAction' topRun name tls args
