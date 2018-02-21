module Main where

import Data.SVD
import Text.Pretty.Simple
import System.Environment
import System.Exit

import Data.List


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "invalid argument\nUSAGE: svdparse [FILES]"
    fs -> parseAndShow fs

parseAndShow :: [String] -> IO()
parseAndShow [] = return ()
parseAndShow (f:fs) = do
  res <- parseSVD f
  case res of
    Left e -> do
      putStrLn ("No parse of " ++ f ++ " error was " ++ e)
      exitFailure
    Right x -> do
      --pPrint x
      putStrLn $ ppDevice x
      let
          periph = "RCC"
          reg = "CFGR"
          p = filter ((== periph) . periphName) $ devicePeripherals x
          r = filter ((== reg) . regName) $ periphRegisters $ head p

      --pPrint $ procFields $ regFields $ head r
      putStrLn $ ppRegister $ head r

      print $ maximum $ mapDevFields (length . fieldName) x
      print $ take 10 $ reverse $ sortOn fst $ mapDevFields (\x -> (length $  fieldName x, fieldName x)) x

  parseAndShow fs


