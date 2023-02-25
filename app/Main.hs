module Main where

import System.Directory
import System.IO
import System.Posix.Process
import System.Process

-- entry point
main :: IO ()
main = do
  putStrLn "Welcome to the Haskell SHell!"
  interactWithCwd runner

-- commands
cd :: [String] -> IO ()
cd [dir] = setCurrentDirectory dir
cd _ = putStrLn "Usage: cd <path>"

-- handles a single line from stdin
-- it checks whether we should execute an inbuilt function like 'cd'
-- or a normal program like 'ls'
runner :: String -> IO ()
runner str = dispatch (ownSplit ' ' str)
  where
    dispatch :: [String] -> IO ()
    dispatch [] = return ()
    dispatch ("cd" : args) = cd args
    dispatch xs = executeProgram xs

-- splits a string on a given delimeter (on each occurrence of delim)
ownSplit :: Char -> [Char] -> [[Char]]
ownSplit delim xs = splitHelper delim xs []
  where
    splitHelper :: Char -> [Char] -> [Char] -> [[Char]]
    splitHelper delim [] [] = []
    splitHelper delim [] drag = [drag]
    splitHelper delim (x : xs) [] | delim == x = splitHelper delim xs []
    splitHelper delim (x : xs) drag | delim == x = drag : splitHelper delim xs []
    splitHelper delim (x : xs) drag = splitHelper delim xs (drag ++ [x])

-- executes a program, takes in the whole commandline (including the programs name at cmdline[0])
executeProgram :: [String] -> IO ()
executeProgram [] = return ()
executeProgram (prog : args) = do
  (_, _, _, childHandle) <- createProcess cp
  waitForProcess childHandle
  return ()
  where
    cp = proc prog args

-- like preludes 'interact' but prints the CWD and takes a function that does IO itself
interactWithCwd :: (String -> IO ()) -> IO ()
interactWithCwd fun = printCwd >> getLine >>= fun >> interactWithCwd fun

-- prints the CWD in shell typical format
printCwd :: IO ()
printCwd = do
  cwd <- getCurrentDirectory
  putStr $ cwd ++ " > "
  hFlush stdout
