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
    ( copyFile,
      doesDirExist,
      doesFileExist,
      ensureDir,
      listDir,
      listDirRecur,
      removeDirRecur)
import qualified GHC.Arr as Prelude
import Control.Monad ( foldM, filterM )
import qualified Path as Path.IO
import Data.List (isPrefixOf, isSuffixOf)
import qualified System.Directory
import qualified System.FilePath
import qualified Debug.Trace as Debug


-- Basics

echo :: PipeFunc
echo args = liftIO $ sequence_ (putStrLn . show' <$> args) >> pure ValUnit
  where show' (ValStr x) = x
        show' x = show x

dirty :: PipeFunc
dirty [] = do
  modifyCurrentTask $ \t -> t { Language.PipeScript.Interpreter.Context.dirty = True }
  return ValUnit
dirty _ = error "dirty: not implemented"

discard :: PipeFunc
discard [] = do
  modify $ \s -> s { curTask = Nothing }
  return ValUnit
discard _ = evalError "discard: takes no arguments"

input :: PipeFunc
input (ValAbsPath x : args) = do
  modifyCurrentTask $ \t -> t { inputFiles = toFilePath x : inputFiles t }
  input args
input (ValStr x : args) = do
  curDir <- currentWorkAbsDir
  let absInputFile = curDir System.FilePath.</> x
  modifyCurrentTask $ \t -> t { inputFiles = absInputFile : inputFiles t }
  input args
input (ValSymbol x : args) = input (ValStr x : args)
input [] = pure ValUnit
input args = evalError $ "input: takes 1 or more arguments, but got " ++ show args

output :: PipeFunc
output (ValAbsPath x : args) = do
  modifyCurrentTask $ \t -> t { outputFiles = toFilePath x : outputFiles t }
  output args
output (ValStr x : args) = do
  curDir <- currentWorkAbsDir
  let absOutputFile = curDir System.FilePath.</> x
  modifyCurrentTask $ \t -> t { outputFiles = absOutputFile : outputFiles t }
  output args
output (ValSymbol x : args) = output (ValStr x : args)
output [] = pure ValUnit
output args = evalError $ "output: takes 1 or more arguments, but got " ++ show args


-- Number Operations

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


-- Boolean Operations

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


-- List & String Operations

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


-- File Operations

writeText :: PipeFunc
writeText [ValStr content, ValAbsPath path] = do
  liftIO $ Prelude.writeFile (Path.IO.fromAbsFile path) content
  return ValUnit
writeText [ValStr content, ValStr relPath] = do
  dir <- currentWorkAbsDir
  let path = dir System.FilePath.</> relPath
  liftIO $ Prelude.writeFile path content
  return ValUnit
writeText [ValList x, path] = do
  writeText [ValStr $ unlines $ fmap value2Str x, path]
writeText [x, path] = do
  writeText [ValStr $ value2Str x, path]
writeText _ = evalError "write-text: takes 2 arguments"

changeExtension :: PipeFunc
changeExtension [ValStr ext, ValAbsPath x] = ValAbsPath <$> ext `replaceExtension` x
changeExtension [ValStr ext, ValStr file] = do
  return $ ValStr $ file System.FilePath.-<.> ext
changeExtension [ValStr ext, ValSymbol file] = changeExtension [ValStr ext, ValStr file]
changeExtension [ValSymbol ext, b] = changeExtension [ValStr ext, b]
changeExtension args = evalError $ "change-extension: invalid arguments: " ++ show args

processPathSuffix :: [Char] -> [Char]
processPathSuffix x
      | last x == '/' || last x == '\\' = processPathSuffix $ init x
      | otherwise = x

combinePath :: PipeFunc
combinePath (ValAbsPath x : args) = do
  (ValStr rel) <- combinePath args
  rel' <- parseRelFile rel
  x' <- parseAbsDir $ toFilePath x
  pure $ ValAbsPath (x' </> rel')
combinePath args = foldM combinePath' (ValStr ".") args
  where
    combinePath' (ValStr x) (ValStr y) = do
      x' <- parseRelDir x
      y' <- parseRelFile $ processPathSuffix y
      pure $ ValStr $ toFilePath (x' </> y')
    combinePath' (ValStr x) (ValSymbol y) = combinePath' (ValStr x) (ValStr y)
    combinePath' (ValSymbol x) y = combinePath' (ValStr x) y
    combinePath' _ _ = evalError $ "combine-path: invalid arguments: " ++ show args

fileExists :: PipeFunc
fileExists [ValStr file] = do
  dir <- currentWorkDir
  exists <- liftIO $ System.Directory.doesFileExist $ dir System.FilePath.</> file
  return $ ValBool exists
fileExists [ValAbsPath file] = do
  exists <- liftIO $ System.Directory.doesFileExist $ toFilePath file
  pure $ ValBool exists
fileExists [ValSymbol file] = fileExists [ValStr file]
fileExists args = evalError $ "file-exists: invalid arguments: " ++ show args

dirExists :: PipeFunc
dirExists [ValAbsPath path] = do
  path' <- parseAbsDir $ toFilePath path
  exists <- liftIO $ doesDirExist path'
  pure $ ValBool exists
dirExists [ValStr dir] = do
  cd <- currentWorkDir
  exists <- liftIO $ System.Directory.doesDirectoryExist $ cd System.FilePath.</> dir
  pure $ ValBool exists
dirExists [ValSymbol s] = dirExists [ValStr s]
dirExists args = evalError $ "dir-exists: invalid arguments: " ++ show args


getFiles' :: (FilePath -> IO Bool) -> PipeFunc
getFiles' f [ValAbsPath path] = do
  let path' = toFilePath path
  content <- liftIO $ System.Directory.listDirectory path'
  content' <- liftIO $ filterM (f . (path' System.FilePath.</>)) content
  pure $ ValList $ fmap ValStr content'
getFiles' f [ValStr path] = do
  cd <- currentWorkAbsDir
  let path' = cd System.FilePath.</> path
  content <- liftIO $ System.Directory.listDirectory path'
  content' <- liftIO $ filterM (f . (path' System.FilePath.</>)) content
  pure $ ValList $ fmap ValStr content'
getFiles' _ _ = evalError "get-files: takes 1 argument"

getFiles :: PipeFunc
getFiles = getFiles' System.Directory.doesFileExist

getDirs :: PipeFunc
getDirs = getFiles' System.Directory.doesDirectoryExist

ensureDir :: PipeFunc
ensureDir [ValStr dir] = do
  cd <- currentWorkAbsDir
  liftIO $ System.Directory.createDirectoryIfMissing True (cd System.FilePath.</> dir)
  return ValUnit
ensureDir [ValAbsPath dir] = do
  dir <- parseAbsDir $ toFilePath dir
  liftIO $ Path.IO.ensureDir dir
  return ValUnit
ensureDir [ValSymbol dir] =
  Language.PipeScript.Interpreter.PipeLibrary.ensureDir [ValStr dir]
ensureDir args = evalError $ "ensure-dir: invalid arguments: " ++ show args

copyFile :: PipeFunc
copyFile [ValAbsPath src, dst] = do
  Language.PipeScript.Interpreter.PipeLibrary.copyFile [ValStr $ toFilePath src, dst]
copyFile [src, ValAbsPath dst] = do
  Language.PipeScript.Interpreter.PipeLibrary.copyFile [src, ValStr $ toFilePath dst]
copyFile [ValStr src, ValStr dst] = do
  cd <- currentWorkAbsDir
  let src' = cd System.FilePath.</> src
      dst' = cd System.FilePath.</> dst
  liftIO $ System.Directory.copyFile src' dst'
  return ValUnit
copyFile [ValSymbol src, x] =
  Language.PipeScript.Interpreter.PipeLibrary.copyFile [ValStr src, x]
copyFile [x, ValSymbol dst] =
  Language.PipeScript.Interpreter.PipeLibrary.copyFile [x, ValStr dst]
copyFile args = evalError $ "copy-file: invalid arguments: " ++ show args

deleteDir :: PipeFunc
deleteDir [ValAbsPath dir] = do
  dir' <- parseAbsDir $ toFilePath dir
  liftIO $ Path.IO.removeDirRecur dir'
  return ValUnit
deleteDir [ValStr dir] = do
  cd <- currentWorkAbsDir
  liftIO $ System.Directory.removeDirectoryRecursive (cd System.FilePath.</> dir)
  return ValUnit
deleteDir [ValSymbol dir] = do
  Language.PipeScript.Interpreter.PipeLibrary.deleteDir [ValStr dir]
deleteDir args = evalError $ "delete-dir: invalid arguments: " ++ show args

deleteFile :: PipeFunc
deleteFile [ValAbsPath file] = do
  liftIO $ System.Directory.removeFile $ toFilePath file
  return ValUnit
deleteFile [ValStr file] = do
  cd <- currentWorkAbsDir
  liftIO $ System.Directory.removeFile $ cd System.FilePath.</> file
  return ValUnit
deleteFile [ValSymbol x] = do
  Language.PipeScript.Interpreter.PipeLibrary.deleteFile [ValStr x]
deleteFile args = evalError $ "delete-file: invalid arguments: " ++ show args

asAbsPath :: PipeFunc
asAbsPath [ValStr absPath] = ValAbsPath <$> parseAbsFile absPath
asAbsPath args = evalError $ "as-abs-path: invalid arguments: " ++ show args

toAbsPath :: PipeFunc
toAbsPath [ValStr dir] = do
  cd <- currentWorkAbsDir
  r <- parseAbsFile $ cd System.FilePath.</> dir
  pure $ ValAbsPath r
toAbsPath [ValSymbol x] = toAbsPath [ValStr x]
toAbsPath args = evalError $ "to-abs-path: invalid arguments: " ++ show args

filename :: PipeFunc
filename [ValAbsPath path] = pure $ ValStr $ toFilePath $ Path.IO.filename path
filename [ValStr path] = do
  path' <- liftIO $ parseRelFile path
  pure $ ValStr $ toFilePath $ Path.IO.filename path'
filename args = evalError $ "filename: invalid arguments: " ++ show args

startsWith :: PipeFunc
startsWith [ValStr prefix, ValStr str] = pure $ ValBool $ prefix `isPrefixOf` str
startsWith [prefix, ValAbsPath p] = startsWith [prefix, ValStr $ toFilePath p]
startsWith args = evalError $ "starts-with: invalid arguments: " ++ show args

endsWith :: PipeFunc
endsWith [ValStr suffix, ValStr str] = pure $ ValBool $ suffix `isSuffixOf` str
endsWith [suffix, ValAbsPath p] = endsWith [suffix, ValStr $ toFilePath p]
endsWith args = evalError $ "ends-with: invalid arguments: " ++ show args


-- Load Libraries

loadLibrary :: Context -> Context
loadLibrary c = c {funcs = fromList libi `union` funcs c}
  where
    libi = fmap (first Identifier) lib
    lib =
      [ ("echo", echo),
        ("dirty", Language.PipeScript.Interpreter.PipeLibrary.dirty),
        ("discard", discard),
        ("input", input),
        ("output", output),
        ("add", add),
        ("and", Language.PipeScript.Interpreter.PipeLibrary.and),
        ("or", Language.PipeScript.Interpreter.PipeLibrary.or),
        ("not", Language.PipeScript.Interpreter.PipeLibrary.not),
        ("head", Language.PipeScript.Interpreter.PipeLibrary.head),
        ("tail", Language.PipeScript.Interpreter.PipeLibrary.tail),
        ("is-empty", isEmpty),
        ("length", Language.PipeScript.Interpreter.PipeLibrary.length),
        ("nth", nth),
        ("fst", Language.PipeScript.Interpreter.PipeLibrary.fst),
        ("snd", Language.PipeScript.Interpreter.PipeLibrary.snd),
        ("trd", trd),
        ("mul", mul),
        ("sub", sub),
        ("div", Language.PipeScript.Interpreter.PipeLibrary.div),
        ("range", Language.PipeScript.Interpreter.PipeLibrary.range),
        ("write-text", writeText),
        ("change-extension", Language.PipeScript.Interpreter.PipeLibrary.changeExtension),
        ("combine-path", Language.PipeScript.Interpreter.PipeLibrary.combinePath),
        ("file-exists", fileExists),
        ("dir-exists", dirExists),
        ("get-files", getFiles),
        ("get-dirs", getDirs),
        ("ensure-dir", Language.PipeScript.Interpreter.PipeLibrary.ensureDir),
        ("copy-file", Language.PipeScript.Interpreter.PipeLibrary.copyFile),
        ("delete-dir", deleteDir),
        ("delete-file", deleteFile),
        ("to-abs-path", toAbsPath),
        ("as-abs-path", asAbsPath),
        ("filename",  Language.PipeScript.Interpreter.PipeLibrary.filename),
        ("starts-with", startsWith),
        ("ends-with", endsWith)
      ]