{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TupleSections #-}

module Language.PipeScript.Interpreter.Context
  ( Value (..),
    Context (..),
    Interpreter (..),
    createContext,
    valueFromConstant,
    setVariable,
    setVariables,
    putConstant,
    variableScope,
    getVariable,
    getVariables,
    getVariableEnvs,
    Task (..),
    createTask,
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
import Data.List ( groupBy )
import Data.HashSet (HashSet, empty)

data Value
  = ValInt Int
  | ValStr String
  | ValAbsDir (Path Abs Dir)
  | ValNum Double
  | ValBool Bool
  | ValList [Value]
  | ValUnit
  deriving (Eq)

instance Show Value where
  show (ValInt x) = show x
  show (ValStr x) = show x
  show (ValAbsDir x) = show x
  show (ValNum x) = show x
  show (ValBool True) = "true"
  show (ValBool False) = "false"
  show (ValList x) = show x
  show ValUnit = show ()

type PipeFunc = [Value] -> Interpreter Value

data Task = Task
  { inputFile :: [Path Abs File],
    outputFile :: [Path Abs File],
    forceDirty :: Bool,
    operationName :: String,
    arguments :: [Value],
    context :: Context
  }

createTask :: String -> [Value] -> Task
createTask operationName arguments =
  Task
    { inputFile = [],
      outputFile = [],
      forceDirty = False,
      operationName = operationName,
      arguments = arguments,
      context = undefined
    }

data Context = Context
  { topLevels :: HashMap String [(Script, TopLevel)],
    variables :: HashMap Variable Value,
    funcs :: HashMap Identifier PipeFunc,
    actionInited :: HashSet String,
    curScript :: Script,
    curTopLevel :: TopLevel,
    curStatement :: Statement,
    verbose :: Bool,
    curTask :: Maybe Task,
    tasks :: [Task],
    isPreRun :: Bool
  }

type Interpreter = StateT Context IO

createContext :: Bool -> [Script] -> Context
createContext verbose scripts =
  Context
    { topLevels = fromList $ fmap (\x -> (, x) $ name' $ head x) groups,
      variables = Data.HashMap.Strict.empty,
      funcs = Data.HashMap.Strict.empty,
      actionInited = Data.HashSet.empty,
      curScript = undefined,
      curTopLevel = undefined,
      curStatement = undefined,
      verbose = verbose,
      curTask = Nothing,
      tasks = [],
      isPreRun = True
    }
  where name' :: (Script, TopLevel) -> String
        name' (_, Include _) = error "Here is an include toplevel!"
        name' (_, ActionDefination b) = show $ name b
        name' (_, OperationDefination _ b) = show $ name b
        name' (_, TaskDefination b) = show $ name b
        convert script = (script, ) <$> scriptAST script
        groups :: [[(Script, TopLevel)]]
        groups = groupBy (\x y -> name' x == name' y) (scripts >>= convert)

valueFromConstant :: Constant -> Value
valueFromConstant = \case
  ConstInt x -> ValInt x
  ConstStr x -> ValStr x
  ConstNum x -> ValNum x
  ConstBool x -> ValBool x

setVariable :: Variable -> Value -> Interpreter ()
setVariable name val =
  modify $ \c -> c {variables = Data.HashMap.Strict.insert name val $ variables c}

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
getVariable v = do
  vars <- variables <$> get
  case Data.HashMap.Strict.lookup v vars of
    Just x -> return x
    Nothing -> liftIO $ ValStr <$> getEnv vn
  where
    (Variable (Identifier vn)) = v

getVariables :: Interpreter [(Variable, Value)]
getVariables = toList . variables <$> get

getVariableEnvs :: Interpreter [(Variable, Value)]
getVariableEnvs = do
  vars <- getVariables
  envs <- liftIO getEnvironment
  return $ ((\(name, x) -> (Variable $ Identifier name, ValStr x)) <$> envs) ++ vars

putFunc :: Identifier -> PipeFunc -> Interpreter ()
putFunc name f = modify $ \c -> c {funcs = Data.HashMap.Strict.insert name f $ funcs c}
