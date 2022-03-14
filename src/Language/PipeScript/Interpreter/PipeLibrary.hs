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
import Control.Monad (foldM)
import qualified Path as Path.IO
import Data.List (isPrefixOf, isSuffixOf)


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
  modifyCurrentTask $ \t -> t { inputFiles = x : inputFiles t }
  input args
input (ValStr x : args) = do
  curDir <- currentWorkAbsDir
  inputFile <- parseRelFile x
  absInputFile <- liftIO $ pure $ curDir </> inputFile
  input (ValAbsPath absInputFile : args)
input (ValSymbol x : args) = input (ValStr x : args)
input [] = pure ValUnit
input args = evalError $ "input: takes 1 or more arguments, but got " ++ show args

output :: PipeFunc
output (ValAbsPath x : args) = do
  modifyCurrentTask $ \t -> t { outputFiles = x : outputFiles t }
  output args
output (ValStr x : args) = do
  curDir <- currentWorkAbsDir
  outputFile <- parseRelFile x
  absOutputFile <- liftIO $ pure $ curDir </> outputFile
  output (ValAbsPath absOutputFile : args)
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

changeExtension :: PipeFunc
changeExtension [ValStr ext, ValAbsPath x] = ValAbsPath <$> ext `replaceExtension` x
changeExtension [ValStr ext, ValStr file] = do
  path <- parseRelFile file
  ValStr . toFilePath <$> ext `replaceExtension` path
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
  dir <- currentWorkAbsDir
  path <- parseRelFile file
  fileExists [ValAbsPath $ dir </> path]
fileExists [ValAbsPath file] = do
  exists <- liftIO $ doesFileExist file
  pure $ ValBool exists
fileExists [ValSymbol file] = fileExists [ValStr file]
fileExists args = evalError $ "file-exists: invalid arguments: " ++ show args

dirExists :: PipeFunc
dirExists [ValAbsPath path] = do
  path' <- parseAbsDir $ toFilePath path
  exists <- liftIO $ doesDirExist path'
  pure $ ValBool exists
dirExists [ValStr dir] = do
  cd <- currentWorkAbsDir
  dir <- parseRelDir dir
  exists <- liftIO $ doesDirExist (cd </> dir)
  pure $ ValBool exists
dirExists [ValSymbol s] = dirExists [ValStr s]
dirExists args = evalError $ "dir-exists: invalid arguments: " ++ show args

getFilesBase :: (Path Abs Dir -> IO ([Path Abs Dir], [Path Abs File])) -> PipeFunc
getFilesBase lsDir [ValSymbol dir] = getFilesBase lsDir [ValStr dir]
getFilesBase lsDir [ValAbsPath path] = do
  path' <- parseAbsDir $ toFilePath path
  (_, files) <- liftIO $ lsDir path'
  files <- mapM (makeRelative path') files
  pure $ ValList $ fmap (ValStr . toFilePath) files
getFilesBase lsDir [ValStr dir] = do
  cd <- currentWorkAbsDir
  dir <- parseRelDir dir
  files <- liftIO $ Prelude.snd <$> lsDir (cd </> dir)
  filesRel <- mapM (makeRelative $ cd </> dir) files
  return $ ValList $ fmap (ValStr . toFilePath) filesRel
getFilesBase _ args = evalError $ "get-files: invalid arguments: " ++ show args

getFiles :: PipeFunc
getFiles = getFilesBase listDir

allFiles :: PipeFunc
allFiles = getFilesBase listDirRecur

getDirsBase :: (Path Abs Dir -> IO ([Path Abs Dir], [Path Abs File])) -> PipeFunc
getDirsBase lsDir [ValAbsPath path] = do
  path' <- parseAbsDir $ toFilePath path
  (dirs, _) <- liftIO $ lsDir path'
  dirs' <- liftIO $ sequence (parseAbsFile . toFilePath <$> dirs)
  pure $ ValList $ ValAbsPath <$> dirs'
getDirsBase lsDir [ValSymbol dir] = getDirsBase lsDir [ValStr dir]
getDirsBase lsDir [ValStr dir] = do
  cd <- currentWorkAbsDir
  dir <- parseRelDir dir
  files <- liftIO $ Prelude.fst <$> lsDir (cd </> dir)
  filesRel <- mapM (makeRelative $ cd </> dir) files
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
ensureDir [ValAbsPath dir] = do
  dir <- parseAbsDir $ toFilePath dir
  liftIO $ Path.IO.ensureDir dir
  return ValUnit
ensureDir [ValSymbol dir] = 
  Language.PipeScript.Interpreter.PipeLibrary.ensureDir [ValStr dir]
ensureDir args = evalError $ "ensure-dir: invalid arguments: " ++ show args

copyFile :: PipeFunc
copyFile [ValAbsPath src, ValAbsPath dst] = do
  liftIO $ Path.IO.copyFile src dst
  return ValUnit
copyFile [ValStr src, ValAbsPath dst] = do
  src' <- parseRelFile src
  cd <- currentWorkAbsDir
  Language.PipeScript.Interpreter.PipeLibrary.copyFile 
    [ValAbsPath (cd </> src'), ValAbsPath dst]
copyFile [ValStr src, ValStr dst] = do
  dst' <- parseRelFile dst
  cd <- currentWorkAbsDir
  Language.PipeScript.Interpreter.PipeLibrary.copyFile 
    [ValStr src, ValAbsPath (cd </> dst')]
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
  dir <- parseRelDir dir
  cd <- currentWorkAbsDir
  liftIO $ Path.IO.removeDir (cd </> dir)
  return ValUnit
deleteDir [ValSymbol dir] = do
  Language.PipeScript.Interpreter.PipeLibrary.deleteDir [ValStr dir]
deleteDir args = evalError $ "delete-dir: invalid arguments: " ++ show args

deleteFile :: PipeFunc
deleteFile [ValAbsPath file] = do
  liftIO $ Path.IO.removeFile file
  return ValUnit
deleteFile [ValStr file] = do
  file <- parseRelFile file
  cd <- currentWorkAbsDir
  deleteFile [ValAbsPath (cd </> file)]
deleteFile [ValSymbol x] = do
  Language.PipeScript.Interpreter.PipeLibrary.deleteFile [ValStr x]
deleteFile args = evalError $ "delete-file: invalid arguments: " ++ show args

asAbsPath :: PipeFunc
asAbsPath [ValStr absPath] = ValAbsPath <$> parseAbsFile absPath
asAbsPath args = evalError $ "as-abs-path: invalid arguments: " ++ show args

toAbsPath :: PipeFunc
toAbsPath [ValStr dir] = do
  dir <- parseRelFile $ processPathSuffix dir
  cd <- currentWorkAbsDir
  pure $ ValAbsPath (cd </> dir)
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
        ("change-extension", Language.PipeScript.Interpreter.PipeLibrary.changeExtension),
        ("combine-path", Language.PipeScript.Interpreter.PipeLibrary.combinePath),
        ("file-exists", fileExists),
        ("dir-exists", dirExists),
        ("get-files", getFiles),
        ("all-files", allFiles),
        ("get-dirs", getDirs),
        ("all-dirs", allDirs),
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