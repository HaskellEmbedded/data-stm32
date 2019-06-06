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
  pPrint res
  parseAndShow fs


