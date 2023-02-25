module Main where

import Control.Monad.State
import Data.Maybe (fromMaybe)
import System.Directory
import System.IO
import System.Process
import System.Process.Internals (ProcessHandle (ProcessHandle))

data ShellState = ShellState
  { history :: [String],
    processes :: [(ProcessHandle, String)],
    workingDir :: FilePath
  }

type ShellMonad = StateT ShellState IO

-- entry point
main :: IO ()
main = do
  putStrLn "Welcome to the Haskell SHell!"
  cwd <- getCurrentDirectory
  evalStateT runner (ShellState [] [] cwd)

-- commands
cd :: [String] -> ShellMonad ()
cd [dir] = do
  liftIO $ setCurrentDirectory dir
  cwd <- liftIO getCurrentDirectory
  modify (\state -> state {workingDir = cwd})
cd _ = liftIO $ putStrLn "Usage: cd <path>"

jobs :: [String] -> ShellMonad ()
jobs [] = do
  jobs <- gets processes
  liftIO $ mapM_ printJob jobs
  where
    printJob :: (ProcessHandle, String) -> IO ()
    printJob (proc, cmdLine) = do
      pidM <- getPid proc
      let pid = fromMaybe (-1) pidM
      putStrLn $ show pid ++ ": " ++ cmdLine
jobs _ = liftIO $ putStrLn "Usage: jobs"

-- splits a string on a given delimeter (on each occurrence of delim)
parse :: Char -> [Char] -> ([[Char]], Bool)
parse delim xs = splitHelper delim xs []
  where
    splitHelper :: Char -> [Char] -> [Char] -> ([[Char]], Bool)
    splitHelper delim [] [] = ([], False)
    splitHelper delim [] drag = ([drag], False)
    splitHelper delim "&" [] = ([], True)
    splitHelper delim "&" drag = ([drag], True)
    splitHelper delim (x : xs) [] | delim == x = splitHelper delim xs []
    splitHelper delim (x : xs) drag | delim == x = (drag : strs, background)
      where
        (strs, background) = splitHelper delim xs []
    splitHelper delim (x : xs) drag = splitHelper delim xs (drag ++ [x])

-- executes a program, takes in the whole commandline (including the programs name at cmdline[0])
executeProgram :: [String] -> ShellMonad ()
executeProgram [] = return ()
executeProgram (prog : args) = do
  (_, _, _, childHandle) <- liftIO $ createProcess cp
  liftIO $ waitForProcess childHandle
  return ()
  where
    cp = proc prog args

-- executes a program as background task, takes in the whole commandline (including the programs name at cmdline[0])
executeProgramBackground :: [String] -> ShellMonad ()
executeProgramBackground [] = liftIO $ putStrLn "hsh: '&' without command is invalid!"
executeProgramBackground (prog : args) = do
  (_, _, _, childHandle) <- liftIO $ createProcess cp
  procs <- gets processes
  modify (\state -> state {processes = (childHandle, unwords (prog : args)) : procs})
  where
    cp = proc prog args

-- like preludes 'interact' but prints the CWD and takes a function that does IO itself
runner :: ShellMonad ()
runner = do
  checkJobs
  printCwd
  str <- liftIO getLine
  dispatch $ parse ' ' str
  runner
  where
    dispatch :: ([String], Bool) -> ShellMonad ()
    dispatch ("cd" : args, _) = cd args
    dispatch ("jobs" : args, _) = jobs args
    dispatch (xs, True) = executeProgramBackground xs
    dispatch (xs, False) = executeProgram xs

-- prints the CWD in shell typical format
printCwd :: ShellMonad ()
printCwd = do
  cwd <- gets workingDir
  liftIO $ putStr $ cwd ++ " > "
  liftIO $ hFlush stdout

checkJobs :: ShellMonad ()
checkJobs = do
  jobs <- gets processes
  newJobs <- liftIO $ filterM filterProcess jobs
  modify (\state -> state {processes = newJobs})
  where
    filterProcess :: (ProcessHandle, String) -> IO Bool
    filterProcess (proc, cmdline) = do
      ecM <- getProcessExitCode proc
      case ecM of
        Nothing -> return True
        Just exitCode -> putStrLn ("[" ++ cmdline ++ "] exited with code " ++ show exitCode) >> return False
