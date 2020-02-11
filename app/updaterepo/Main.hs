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

fs = [
    "cabal.project"
  , "devices.data"
  , "ivory-bsp-stm32.cabal"
  , "LICENSE"
  , "README.md"
  ]

ds = [ "src", "support" ]

main = do
  forM_ ds $ \d -> rmtree ("repo" </> d)

  forM_ ds $ \d -> cptree ("data" </> d) ("repo" </> d)
  forM_ fs $ \f -> cp ("data" </> f) ("repo" </> f)

  cd "repo"
  stdout $ inshell "git diff" empty
  stdout $ inshell "git status" empty

  return ()

fpToString = T.unpack . format fp
