{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Language.PipeScript.Interpreter.Context
  ( Value (..),
    Context (..),
    Interpreter (..),
    createContext,
    value2Str,
    valueFromConstant,
    setVariable,
    setVariables,
    putConstant,
    variableScope,
    getVariable,
    getVariables,
    getVariableEnvs,
    Task (..),
    PipeFunc,
    createTask,
    modifyCurrentTask,
    currentWorkDir,
    currentWorkAbsDir,
    run
  )
where

import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Strict
import Data.HashMap.Strict
import Data.Hashable (Hashable (hashWithSalt))
import Language.PipeScript
import Language.PipeScript.Parser
import Path
import System.Environment (getEnv, getEnvironment)
import Data.List ( groupBy, sortBy )
import Path.IO (getCurrentDir, AnyPath (makeRelative))
import Data.Maybe (fromMaybe)
import qualified System.FilePath

data Value
  = ValInt Int
  | ValStr String
  | ValSymbol String
  | ValAbsPath (Path Abs File)
  | ValNum Double
  | ValBool Bool
  | ValList [Value]
  | ValUnit
  deriving (Eq)

instance Show Value where
  show (ValInt x) = show x
  show (ValStr x) = "\"" ++ x ++ "\""
  show (ValSymbol x) = x
  show (ValAbsPath x) = toFilePath x
  show (ValNum x) = show x
  show (ValBool True) = "true"
  show (ValBool False) = "false"
  show (ValList x) = show x
  show ValUnit = show ()

value2Str :: Value -> String
value2Str (ValInt x) = show x
value2Str (ValStr x) = x
value2Str (ValSymbol x) = x
value2Str (ValAbsPath x) = toFilePath x
value2Str (ValNum x) = show x
value2Str (ValBool True) = "true"
value2Str (ValBool False) = "false"
value2Str (ValList x) = show x
value2Str ValUnit = show ()

type PipeFunc = [Value] -> Interpreter Value

data Task = Task
  { inputFiles :: [FilePath],   -- absolute path
    outputFiles :: [FilePath],  -- absolute path
    dirty :: Bool,
    operationName :: String,
    arguments :: [Value],
    context :: Context
  }

instance Eq Task where
  a == b =
    inputFiles a == inputFiles b &&
    outputFiles a == outputFiles b &&
    dirty a == dirty b &&
    operationName a == operationName b &&
    arguments a == arguments b

createTask :: String -> [Value] -> Task
createTask operationName arguments =
  Task
    { inputFiles = [],
      outputFiles = [],
      dirty = False,
      operationName = operationName,
      arguments = arguments,
      context = undefined
    }

data Context = Context
  { topLevels :: HashMap String [(Script, TopLevel)],
    variables :: HashMap Variable Value,
    funcs :: HashMap Identifier PipeFunc,
    curScript :: Script,
    curTopLevel :: TopLevel,
    verbose :: Bool,
    curTask :: Maybe Task,
    tasks :: [Task],
    taskRunner :: [Task] -> IO (),
    isPreRun :: Bool
  }

type Interpreter = StateT Context IO

run :: Interpreter () -> Context -> IO Context
run i c = snd <$> runStateT i c

modifyCurrentTask :: (Task -> Task) -> Interpreter ()
modifyCurrentTask f = do
  modify $ \c -> c {curTask = f <$> curTask c }

currentWorkDir :: Interpreter FilePath
currentWorkDir = do
  script <- curScript <$> get
  pure $ scriptDir script

currentWorkAbsDir :: Interpreter FilePath
currentWorkAbsDir = do
  startupDir <- toFilePath <$> getCurrentDir
  curDir <- currentWorkDir
  return $ startupDir System.FilePath.</> curDir

createContext :: Bool -> [Script] -> ([Task] -> IO ()) -> Context
createContext verbose scripts taskRunner =
  Context
    { topLevels = fromList $ fmap (\x -> (, x) $ name' $ head x) groups,
      variables = empty,
      funcs = empty,
      curScript = undefined,
      curTopLevel = undefined,
      verbose = verbose,
      curTask = Nothing,
      tasks = [],
      taskRunner = taskRunner,
      isPreRun = True
    }
  where name' :: (Script, TopLevel) -> String
        name' (_, Include _) = error "Here is an include toplevel!"
        name' (_, ActionDefination b) = show $ name b
        name' (_, OperationDefination _ b) = show $ name b
        name' (_, TaskDefination b) = show $ name b
        convert script = (script, ) <$> scriptAST script
        groups :: [[(Script, TopLevel)]]
        groups =
          groupBy (\x y -> name' x == name' y) $
            sortBy (\a b -> compare (name' a) (name' b)) (scripts >>= convert)

valueFromConstant :: Constant -> Value
valueFromConstant = \case
  ConstInt x -> ValInt x
  ConstStr x -> ValStr x
  ConstNum x -> ValNum x
  ConstBool x -> ValBool x

setVariable :: Variable -> Value -> Interpreter ()
setVariable name val =
  modify $ \c -> c {variables = insert name val $ variables c}

setVariables :: [(Variable, Value)] -> Interpreter ()
setVariables [] = return ()
setVariables ((var, val) : ls) = setVariable var val >> setVariables ls

putConstant :: Variable -> Constant -> Interpreter ()
putConstant name = setVariable name . valueFromConstant

variableScope :: Interpreter a -> Interpreter a
variableScope a = do
  vars <- variables <$> get
  result <- a
  modify $ \ctx -> ctx {variables = vars}
  return result

getVariable :: Variable -> Interpreter Value
getVariable (Variable (Identifier "cd")) = do
  rel <- scriptDir . curScript <$> get
  cd <- toFilePath <$> getCurrentDir
  return $ ValStr $ cd System.FilePath.</> rel
getVariable (Variable (Identifier "input")) = do
  t <- curTask <$> get
  cd <- currentWorkAbsDir
  let files = maybe [] inputFiles t
  let files' = System.FilePath.makeRelative cd <$> files
  return $ ValList $ ValStr <$> files'
getVariable (Variable (Identifier "output")) = do
  t <- curTask <$> get
  cd <- currentWorkAbsDir
  let files = System.FilePath.makeRelative cd <$> maybe [] outputFiles t
  return $ ValList $ ValStr <$> files
getVariable v = do
  vars <- variables <$> get
  case vars !? v of
    Just x -> return x
    Nothing -> liftIO $ ValSymbol <$> getEnv vn
  where
    (Variable (Identifier vn)) = v


getVariables :: Interpreter [(Variable, Value)]
getVariables = toList . variables <$> get

getVariableEnvs :: Interpreter [(Variable, Value)]
getVariableEnvs = do
  vars <- getVariables
  envs <- liftIO getEnvironment
  return $ ((\(name, x) -> (Variable $ Identifier name, ValSymbol x)) <$> envs) ++ vars

putFunc :: Identifier -> PipeFunc -> Interpreter ()
putFunc name f = modify $ \c -> c {funcs = insert name f $ funcs c}
