{-# LANGUAGE LambdaCase #-}
module Language.PipeScript.Interpreter.PipeLibrary (loadLibrary) where

import Control.Monad.State.Strict (liftIO)
import Data.Bifunctor (Bifunctor (first))
import Data.HashMap.Strict
import Language.PipeScript
import Language.PipeScript.Interpreter.Context
import Language.PipeScript.Interpreter.Eval
import System.Process (runCommand)
import Control.Monad.State.Class (modify)
import Path
import Path.IO
import qualified GHC.Arr as Prelude

echo :: PipeFunc
echo args = liftIO $ sequence_ (putStrLn . show' <$> args) >> pure ValUnit
  where show' (ValStr x) = x
        show' x = show x

dirty :: PipeFunc
dirty _ = do
  modifyCurrentTask $ \t -> t { forceDirty = True }
  return ValUnit

discard :: PipeFunc
discard _ = do
  modify $ \s -> s { curTask = Nothing }
  return ValUnit

input :: PipeFunc
input args = do
  curDir <- currentWorkAbsDir
  inputFiles' <- sequence (parseRelFile . show <$> args)
  modifyCurrentTask $ \t -> t { inputFiles = fmap (curDir </>) inputFiles' ++ inputFiles t }
  return ValUnit

output :: PipeFunc
output args = do
  curDir <- currentWorkAbsDir
  outputFiles' <- sequence (parseRelFile . show <$> args)
  modifyCurrentTask $ \t -> t { outputFiles = fmap (curDir </>) outputFiles' ++ outputFiles t }
  return ValUnit

numberOperator :: (Double -> Double -> Double) -> (Int -> Int -> Int) -> PipeFunc
numberOperator op opi [] = pure ValUnit
numberOperator op opi [x] = pure x
numberOperator op opi [x,  ValUnit] = pure x
numberOperator op opi [ValUnit, x] = pure x
numberOperator op opi (ValInt x : ValInt y : args) = numberOperator op opi (ValInt (x `opi` y) : args)
numberOperator op opi (ValNum x : ValNum y : args) = numberOperator op opi (ValNum (x `op` y) : args)
numberOperator op opi (ValNum x : ValInt y : args) = numberOperator op opi (ValNum (x `op` fromIntegral y) : args)
numberOperator op opi (ValInt x : ValNum y : args) = numberOperator op opi (ValNum (fromIntegral x `op` y) : args)
numberOperator op opi args = evalError $ "Invalid arguments for number operator: " ++ show args

add :: PipeFunc
add (ValList x : ValList y : args) = add (ValList (x ++ y) : args)
add (ValStr x : ValStr y : args) = add (ValStr (x ++ y) : args)
add args = numberOperator (+) (+) args

mul :: PipeFunc
mul = numberOperator (*) (*)

sub :: PipeFunc
sub = numberOperator (-) (-)

div :: PipeFunc
div = numberOperator (/) Prelude.div

range :: PipeFunc
range [ValInt x, ValInt y] = pure $ ValList $ ValInt <$> Prelude.range (x, y)
range args = evalError $ "Invalid arguments for range: " ++ show args

boolOperator :: (Bool -> Bool -> Bool) -> PipeFunc
boolOperator op [] = pure ValUnit
boolOperator op [x] = pure x
boolOperator op [x, ValUnit] = pure x
boolOperator op [ValUnit, x] = pure x
boolOperator op (ValBool x : ValBool y : args) = boolOperator op (ValBool (x `op` y) : args)
boolOperator op args = evalError $ "bool operator: invalid arguments: " ++ show args

and :: PipeFunc
and = boolOperator (&&)

or :: PipeFunc
or = boolOperator (||)

not :: PipeFunc
not [ValBool x] = pure $ ValBool (Prelude.not x)
not args = evalError $ "not: invalid arguments: " ++ show args

head :: PipeFunc
head [ValList x] = pure $ Prelude.head x
head [ValStr x] = pure $ ValStr [Prelude.head x]
head args = evalError $ "head: invalid arguments: " ++ show args

tail :: PipeFunc
tail [ValList x] = pure $ ValList $ Prelude.tail x
tail [ValStr x] = pure $ ValStr $ Prelude.tail x
tail args = evalError $ "tail: invalid arguments: " ++ show args

isEmpty :: PipeFunc
isEmpty [ValList x] = pure $ ValBool $ Prelude.null x
isEmpty [ValStr x] = pure $ ValBool $ Prelude.null x
isEmpty args = evalError $ "is-empty: invalid arguments: " ++ show args

length :: PipeFunc
length [ValList x] = pure $ ValInt $ Prelude.length x
length [ValStr x] = pure $ ValInt $ Prelude.length x
length args = evalError $ "length: invalid arguments: " ++ show args

nth :: PipeFunc
nth [ValInt y, ValList x] = pure $ x !! y
nth [ValInt y, ValStr x] = pure $ ValStr [x !! y]
nth args = evalError $ "nth: invalid arguments: " ++ show args

fst :: PipeFunc
fst args = nth (ValInt 0 : args)

snd :: PipeFunc
snd args = nth (ValInt 1 : args)

trd :: PipeFunc
trd args = nth (ValInt 2 : args)

changeExtension :: PipeFunc
changeExtension [ValStr ext, ValStr file] = do
  path <- parseRelFile file
  ValStr . toFilePath <$> ext `replaceExtension` path
changeExtension args = evalError $ "change-extension: invalid arguments: " ++ show args

combinePath :: PipeFunc
combinePath [ValStr x, ValStr y] = do
  x' <- parseRelDir x
  y' <- parseRelFile $ processSuffix y
  pure $ ValStr $ toFilePath (x' </> y')
  where 
    processSuffix x
      | last x == '/' || last x == '\\' = processSuffix $ init x
      | otherwise = x
combinePath args = evalError $ "combine-path: invalid arguments: " ++ show args

fileExists :: PipeFunc
fileExists [ValStr file] = do
  dir <- currentWorkAbsDir
  path <- parseRelFile file
  exists <- liftIO $ doesFileExist (dir </> path)
  pure $ ValBool exists
fileExists args = evalError $ "file-exists: invalid arguments: " ++ show args

dirExists :: PipeFunc
dirExists [ValStr dir] = do
  cd <- currentWorkAbsDir
  dir <- parseRelDir dir
  exists <- liftIO $ doesDirExist (cd </> dir)
  pure $ ValBool exists
dirExists args = evalError $ "dir-exists: invalid arguments: " ++ show args

getFilesBase :: (Path Abs Dir -> IO ([Path Abs Dir], [Path Abs File])) -> PipeFunc
getFilesBase lsDir [ValStr dir] = do
  cd <- currentWorkAbsDir
  dir <- parseRelDir dir
  files <- liftIO $ Prelude.snd <$> lsDir (cd </> dir)
  filesRel <- mapM (makeRelative cd) files
  return $ ValList $ fmap (ValStr . toFilePath) filesRel
getFilesBase _ args = evalError $ "get-files: invalid arguments: " ++ show args

getFiles :: PipeFunc
getFiles = getFilesBase listDir

allFiles :: PipeFunc
allFiles = getFilesBase listDirRecur

getDirsBase :: (Path Abs Dir -> IO ([Path Abs Dir], [Path Abs File])) -> PipeFunc
getDirsBase lsDir [ValStr dir] = do
  cd <- currentWorkAbsDir
  dir <- parseRelDir dir
  files <- liftIO $ Prelude.fst <$> lsDir (cd </> dir)
  filesRel <- mapM (makeRelative cd) files
  return $ ValList $ fmap (ValStr . toFilePath) filesRel
getDirsBase _ args = evalError $ "get-dirs: invalid arguments: " ++ show args

getDirs :: PipeFunc
getDirs = getDirsBase listDir

allDirs :: PipeFunc
allDirs = getDirsBase listDirRecur

ensureDir :: PipeFunc
ensureDir [ValStr dir] = do
  dir <- parseRelDir dir
  cd <- currentWorkAbsDir
  liftIO $ Path.IO.ensureDir (cd </> dir)
  return ValUnit
ensureDir args = evalError $ "ensure-dir: invalid arguments: " ++ show args

copyFile :: PipeFunc
copyFile [ValStr src, ValStr dst] = do
  src' <- parseRelFile src
  dst' <- parseRelFile dst
  cd <- currentWorkAbsDir
  liftIO $ Path.IO.copyFile (cd </> src') (cd </> dst')
  return ValUnit
copyFile args = evalError $ "copy-file: invalid arguments: " ++ show args

deleteDir :: PipeFunc
deleteDir [ValStr dir] = do
  dir <- parseRelDir dir
  cd <- currentWorkAbsDir
  liftIO $ Path.IO.removeDir (cd </> dir)
  return ValUnit
deleteDir args = evalError $ "delete-dir: invalid arguments: " ++ show args

deleteFile :: PipeFunc
deleteFile [ValStr file] = do
  file <- parseRelFile file
  cd <- currentWorkAbsDir
  liftIO $ Path.IO.removeFile (cd </> file)
  return ValUnit
deleteFile args = evalError $ "delete-file: invalid arguments: " ++ show args

loadLibrary :: Context -> Context
loadLibrary c = c {funcs = fromList libi `union` funcs c}
  where
    libi = fmap (first Identifier) lib
    lib =
      [ ("echo", echo),
        ("dirty", dirty),
        ("discard", discard),
        ("input", input),
        ("output", output),
        ("add", add),
        ("and", Language.PipeScript.Interpreter.PipeLibrary.and),
        ("or", Language.PipeScript.Interpreter.PipeLibrary.or),
        ("not", Language.PipeScript.Interpreter.PipeLibrary.not),
        ("head", Language.PipeScript.Interpreter.PipeLibrary.head),
        ("tail", Language.PipeScript.Interpreter.PipeLibrary.tail),
        ("isEmpty", isEmpty),
        ("length", Language.PipeScript.Interpreter.PipeLibrary.length),
        ("nth", nth),
        ("fst", Language.PipeScript.Interpreter.PipeLibrary.fst),
        ("snd", Language.PipeScript.Interpreter.PipeLibrary.snd),
        ("trd", trd),
        ("mul", mul),
        ("sub", sub),
        ("div", Language.PipeScript.Interpreter.PipeLibrary.div),
        ("range", Language.PipeScript.Interpreter.PipeLibrary.range),
        ("change-extension", Language.PipeScript.Interpreter.PipeLibrary.changeExtension),
        ("combine-path", Language.PipeScript.Interpreter.PipeLibrary.combinePath),
        ("file-exists", Language.PipeScript.Interpreter.PipeLibrary.fileExists),
        ("dir-exists", Language.PipeScript.Interpreter.PipeLibrary.dirExists),
        ("get-files", Language.PipeScript.Interpreter.PipeLibrary.getFiles),
        ("all-files", Language.PipeScript.Interpreter.PipeLibrary.allFiles),
        ("get-dirs", Language.PipeScript.Interpreter.PipeLibrary.getDirs),
        ("all-dirs", Language.PipeScript.Interpreter.PipeLibrary.allDirs),
        ("ensure-dir", Language.PipeScript.Interpreter.PipeLibrary.ensureDir),
        ("copy-file", Language.PipeScript.Interpreter.PipeLibrary.copyFile),
        ("delete-dir", Language.PipeScript.Interpreter.PipeLibrary.deleteDir),
        ("delete-file", Language.PipeScript.Interpreter.PipeLibrary.deleteFile)
      ]