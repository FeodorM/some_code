module Main where

import System.IO
import System.Directory (getDirectoryContents, removeFile)
import Data.List

main :: IO ()
main = putStrLn "Main called"

hiName :: IO ()
hiName = do
  name <- readName
  putStrLn $ "Hi, " ++ name ++ "!"
  where
    readName :: IO String
    readName = do
      putStr "What is your name?\nName: "
      hFlush stdout
      name <- getLine
      if name == "" then
        readName
      else
        return name

removeFiles :: IO ()
removeFiles = do
  putStr "Substring: "
  hFlush stdout
  substring <- getLine
  if substring == "" then
    putStrLn "Canceled"
  else do
    files <- getDirectoryContents "."
    mapM_ f $ filter (isInfixOf substring) files
      where
        f x = do
          putStrLn $ "Removing file: " ++ x
          removeFile x
