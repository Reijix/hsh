module Main where

import System.Directory
import System.IO
import System.Posix.Process
import System.Process

main :: IO ()
main = do
  putStrLn "Welcome to the Haskell SHell!"
  interactWithCwd echo

echo :: String -> IO String
echo str = do
  out <- executeProgram (ownSplit ' ' str)
  return ""

ownSplit :: Char -> [Char] -> [[Char]]
ownSplit delim xs = splitHelper delim xs []
  where
    splitHelper :: Char -> [Char] -> [Char] -> [[Char]]
    splitHelper delim [] [] = []
    splitHelper delim [] drag = [drag]
    splitHelper delim (x : xs) [] | delim == x = splitHelper delim xs []
    splitHelper delim (x : xs) drag | delim == x = drag : splitHelper delim xs []
    splitHelper delim (x : xs) drag = splitHelper delim xs (drag ++ [x])

executeProgram :: [String] -> IO ()
executeProgram [] = return ()
executeProgram (prog : args) = do
  (_, _, _, childHandle) <- createProcess cp
  waitForProcess childHandle
  return ()
  where
    cp = proc prog args

interactWithCwd :: (String -> IO String) -> IO ()
interactWithCwd fun = do
  printCwd
  line <- getLine
  out <- fun line
  hFlush stdout
  interactWithCwd fun

printCwd :: IO ()
printCwd = do
  cwd <- getCurrentDirectory
  putStr $ cwd ++ " > "
  hFlush stdout
