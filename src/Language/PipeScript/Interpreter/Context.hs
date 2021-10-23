{-# LANGUAGE LambdaCase #-}

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
    getVariableEnvs
  )
where

import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Strict
import Data.HashMap.Strict
import Language.PipeScript
import Language.PipeScript.Parser
import Path
import System.Environment (getEnv, getEnvironment)

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

type PipeFunc = Interpreter [Value] -> Interpreter Value

data Context = Context
  { scripts :: [Script],
    variables :: HashMap Variable Value,
    funcs :: HashMap Identifier PipeFunc,
    curScript :: Script,
    curTopLevel :: TopLevel,
    curStatement :: Statement,
    verbose :: Bool
  }

type Interpreter = StateT Context IO

createContext :: Bool -> [Script] -> Context
createContext verbose scripts =
  Context
    { scripts = scripts,
      variables = empty,
      funcs = empty,
      curScript = undefined,
      curTopLevel = undefined,
      curStatement = undefined,
      verbose = verbose
    }

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
putFunc name f = modify $ \c -> c { funcs = insert name f $ funcs c }

