module Language.PipeScript.Interpreter.Context
  ( Value (..),
    Context,
    Interpreter,
    createContext
  )
where

import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Strict
import Data.HashMap.Strict
import Language.PipeScript
import Language.PipeScript.Parser
import Path

data Value
  = PipeInt Int
  | PipeString String
  | PipeAbsDir (Path Abs Dir)
  | PipeNumber Double
  deriving (Eq, Show)

data Context = Context
  { scripts :: [Script],
    variables :: HashMap Variable Value
  } deriving (Show, Eq)

type Interpreter = StateT Context IO

createContext :: [Script] -> Context
createContext scripts = 
  Context { scripts = scripts, variables = empty }

