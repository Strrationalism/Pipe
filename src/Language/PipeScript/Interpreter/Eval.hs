module Language.PipeScript.Interpreter.Eval (runTopLevelByName, runAfters, evalExpr) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class
import Data.Foldable
import Data.HashMap.Strict (member, (!?))
import qualified Data.HashMap.Strict
import qualified Data.HashSet
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import Language.PipeScript
import Language.PipeScript.Interpreter.Context
import Language.PipeScript.Parser
import Path (toFilePath)
import System.Process hiding (runCommand)
import System.Exit (exitFailure)

evalError :: String -> Interpreter a
evalError x = do
  script <- curScript <$> get
  topLevel <- curTopLevel <$> get
  liftIO . putStrLn $ "In " ++ show (scriptPath script) ++ " (" ++ show topLevel ++ "):"
  liftIO . putStrLn $  "  " ++ x
  liftIO . putStrLn $  ""
  liftIO exitFailure

loadStr :: String -> Interpreter String
loadStr x =
  eval x Nothing
  where
    eval [] Nothing = return ""
    eval [] (Just v) = evalError $ "Invalid string with unclosed variable \"" ++ v ++ "\"."
    eval ('%' : '%' : ls) varState = ('%' :) <$> eval ls varState
    eval ('%' : ls) Nothing = eval ls $ Just ""
    eval (a : ls) Nothing = (a :) <$> eval ls Nothing
    eval ('%' : ls) (Just var) = do
      val <- getVariable $ Variable $ Identifier var
      next <- eval ls Nothing
      return $ show val ++ next
    eval (a : ls) (Just var) = eval ls $ Just $ var ++ [a]

runCommand :: FilePath -> [String] -> Interpreter ExitCode
runCommand command args = do
  workdir <- scriptDir . curScript <$> get
  e <- getVariableEnvs
  let info =
        CreateProcess
          { cmdspec = RawCommand command args,
            cwd = Just $ toFilePath workdir,
            env = Just $ fmap (\(Variable (Identifier var), val) -> (var, show val)) e,
            std_in = Inherit,
            std_out = Inherit,
            std_err = Inherit,
            close_fds = False,
            create_group = False,
            delegate_ctlc = False,
            detach_console = False,
            create_new_console = False,
            new_session = False,
            use_process_jobs = False,
            child_group = Nothing,
            child_user = Nothing
          }

  (_, _, _, process) <- liftIO $ createProcess info
  liftIO $ waitForProcess process

evalExpr :: Expression -> Interpreter Value
evalExpr (IdentifierExpr (Identifier name)) = return $ ValStr name
evalExpr (VariableExpr var) = getVariable var
evalExpr (ConstantExpr (ConstStr str)) = valueFromConstant . ConstStr <$> loadStr str
evalExpr (ConstantExpr c) = return $ valueFromConstant c
evalExpr (ExpandExpr _) = evalError "Expand Expression not supported yet."
evalExpr (DoubleExpandExpr e) = evalExpr $ ExpandExpr e
evalExpr (ApplyExpr (IdentifierExpr (Identifier "let")) [VariableExpr var, expr]) = do
  val <- evalExpr expr
  setVariable var val
  return ValUnit
evalExpr (ApplyExpr (IdentifierExpr (Identifier "set")) _) =
  evalError "Set must has a variable argument and a expression."
evalExpr (ListExpr ls) = ValList <$> sequence (evalExpr <$> ls)
evalExpr (ApplyExpr (IdentifierExpr (Identifier i)) args) = do
  context <- get
  let args' = sequence (evalExpr <$> args)
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
evalExpr (ApplyExpr (ConstantExpr (ConstStr file)) args) = do
  cmd <- loadStr file
  args <- sequence (evalExpr <$> args)
  exitCode <- runCommand cmd $ fmap show args
  case exitCode of
    ExitSuccess -> return ValUnit
    ExitFailure x ->
      evalError $ cmd ++ " with arguments " ++ show args ++ " failed with exitcode " ++ show x ++ "."
evalExpr (ApplyExpr left rights) = do
  left' <- evalExpr left
  case left' of
    ValStr str -> evalExpr $ ApplyExpr (IdentifierExpr $ Identifier str) rights
    x -> evalError $ "Can not apply to \"" ++ show x ++ "\"."

evalStatements :: [Statement] -> Interpreter ()
evalStatements = foldr' ((>>) . evalStatement) (return ())

evalStatement :: Statement -> Interpreter ()
evalStatement stat = do
  prevStatement <- curStatement <$> get
  modify $ \c -> c {curStatement = stat}
  eval stat
  modify $ \c -> c {curStatement = prevStatement}
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

evalTopLevel :: Script -> TopLevel -> [Value] -> Interpreter ()
evalTopLevel script topLevel arguments = do
  prevScript <- curScript <$> get
  prevTopLevel <- curTopLevel <$> get
  modify $ \c -> c {curTopLevel = topLevel, curScript = script}
  eval topLevel
  modify $ \c -> c {curTopLevel = prevTopLevel, curScript = prevScript}
  where
    eval (Include _) = return ()
    eval (ActionDefination b) = evalBlock b
    eval (TaskDefination b) = do
      prevTask <- curTask <$> get
      modify $ \c -> c {curTask = Just $ createTask (show $ name b) arguments}
      evalBlock b
      task <- curTask <$> get
      taskContext <- get
      modify $ \c -> c {curTask = prevTask}
      case task of
        Nothing -> return ()
        Just t -> modify $ \c -> c {tasks = t {context = taskContext} : tasks c}
    eval (OperationDefination t b) = evalBlock b
    evalBlock opBlock = do
      let params = parameters opBlock
      if length params == length arguments
        then variableScope $ do
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
runTopLevels name tls args = do
  tlsToRun >>= run args
  where
    run _ [] = return ()
    run args' ((scr, tp) : ls) = evalTopLevel scr tp args' >> run args' ls
    isTaskBlock (_, TaskDefination _) = True
    isTaskBlock _ = False
    takeTasks = filter isTaskBlock
    takeOps = filter $ not . isTaskBlock
    isBeforeBlock (_, OperationDefination BeforeAction _) = True
    isBeforeBlock _ = False
    takeBefore = filter isBeforeBlock
    isActionBlock (_, ActionDefination _) = True
    isActionBlock _ = False
    takeAction = filter isActionBlock
    tlsToRun
      | isTask tls = do
        preRun <- isPreRun <$> get
        if preRun
          then return $ takeTasks tls
          else return $ takeOps tls
      | isAction tls = do
        initedActions <- actionInited <$> get
        if name `Data.HashSet.member` initedActions
          then return $ takeAction tls
          else do
            run [] $ takeBefore tls
            modify $ \c -> c {actionInited = Data.HashSet.insert name $ actionInited c}
            return $ takeAction tls
      | otherwise = return tls

runTopLevelByName :: String -> [Value] -> Interpreter ()
runTopLevelByName x args = getTopLevels x >>= \x' -> runTopLevels x x' args

runAfters :: Interpreter ()
runAfters = do
  actions <- Data.HashSet.toList . actionInited <$> get
  tls <- join <$> sequence (getTopLevels <$> actions)
  let afters = filter isAfter tls
      isAfter (_, OperationDefination AfterAction _) = True
      isAfter _ = False
      evalTopLevel' (script, topLevel) = evalTopLevel script topLevel []
  sequence_ (evalTopLevel' <$> afters)