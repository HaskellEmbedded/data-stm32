{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Turtle
import System.Exit
import Prelude hiding (FilePath)
import Control.Monad
import qualified Control.Foldl as Fold
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Char8 as B

import Data.SVD hiding (svd, ppPeripheral)

cdmk dir = do
  hasdir <- testdir dir
  when (not hasdir) (mktree dir)
  cd dir

supSvds =
  [ "f0"
  , "f1"
  , "f2"
  , "f3"
  , "f4"
  , "f7"
  , "h7"
  , "l0"
  , "l1"
  , "l4"
  , "l4plus"
  , "g4"
  , "mp1"
  ]

main = do
  cdmk "svds"
  dir <- pwd

  hasGarbage <- testdir "stm"
  when hasGarbage $ do
    rmtree "stm"
    rmtree "work"

  mktree "stm"
  cdmk "work"
  forM_ supSvds $ \n -> do
     shell
      (T.concat ["wget https://www.st.com/resource/en/svd/stm32", n, "_svd.zip"])
      empty

  forM_ supSvds $ \n -> do
     shell
      (T.concat ["unzip -n stm32", n, "_svd.zip"])
      empty

  fs <- fold (find (suffix ".svd") ".") Fold.list

  -- test parse them
  -- svds <- mapM svd1 $ map fpToString fs

  forM_ fs $ \f -> mv f ("../stm/" </> filename f)

  cd dir
  rmtree "work"

  return ()

svd1 f = do
  res <- parseSVD f
  case res of
    Left e -> do
      putStrLn ("No parse of " ++ f ++ " error was " ++ e)
      exitFailure
    Right x -> do
      putStrLn ("Done parsing " ++ f)
      return $ (drop 5 $ deviceName x, x)

fpToString = T.unpack . format fp
