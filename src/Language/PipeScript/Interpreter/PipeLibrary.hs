module Language.PipeScript.Interpreter.PipeLibrary (loadLibrary) where

import Control.Monad.State.Strict (liftIO)
import Data.Bifunctor (Bifunctor (first))
import Data.HashMap.Strict
import Language.PipeScript
import Language.PipeScript.Interpreter.Context
import Language.PipeScript.Interpreter.Eval

echo :: PipeFunc
echo args = liftIO $ sequence_ (putStrLn . show' <$> args) >> pure ValUnit
  where show' (ValStr x) = x
        show' x = show x

loadLibrary :: Context -> Context
loadLibrary c = c {funcs = fromList libi `union` funcs c}
  where
    libi = fmap (first Identifier) lib
    lib =
      [ ("echo", echo)
      ]