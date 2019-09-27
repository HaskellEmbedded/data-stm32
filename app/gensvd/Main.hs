{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Turtle
import System.Exit
import Prelude hiding (FilePath)
import Control.Monad
import qualified Control.Foldl as Fold
import qualified Data.Char as C
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
  -- , "g0" -- FIXME: missing from STs site, should report
  , "g4"
  , "mp1"
  ]

main = do
  cdmk "svds"
  dir <- pwd

  forM_ ["stm", "work"] $ \d -> do
    hasGarbage <- testdir d
    when hasGarbage $ do
      rmtree d

  mktree "stm"
  cdmk "work"
  forM_ supSvds $ \n -> do
     shell
      (T.concat ["wget https://www.st.com/resource/en/svd/stm32", n, "_svd.zip"])
      empty

  -- fetch G0s from rust repo
  shell
    (T.concat ["wget -O stm32g0_svd.zip https://github.com/stm32-rs/stm32-rs/raw/master/svd/vendor/en.stm32g0_svd.zip"])
    empty

  forM_ (supSvds ++ ["g0"]) $ \n -> do
    shell
      (T.concat ["unzip -n stm32", n, "_svd.zip"])
      empty

  fs <- fold (find (suffix ".svd") ".") Fold.list

  -- test parse them
  -- svds <- mapM svd1 $ map fpToString fs


  -- uppercase first 5 letters so e.g. stm32g0 becomes STM32G0
  forM_ fs $ \f -> do
    let namePart = takeWhile (/='.') $ T.unpack $ format fp $ filename f
    mv f ("../stm"
         </> (fromString
            $ (map C.toUpper $ take 6 namePart)
              ++ (drop 6 namePart)
              ++ ".svd"))

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
