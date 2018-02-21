module Main where

import System.Environment
import System.Exit

import Text.Pretty.Simple
import Data.CMX

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "invalid argument\nUSAGE: mxmcu [FILES]"
    fs -> parseAndShow fs

parseAndShow :: [String] -> IO()
parseAndShow [] = return ()
parseAndShow (f:fs) = do
  res <- parseMCU f
  case res of
    Left e -> do
      putStrLn ("No parse of " ++ f ++ " error was " ++ e)
      exitFailure
    Right x -> pPrint x

  parseAndShow fs


