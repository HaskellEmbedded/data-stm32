{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (readFile, putStrLn, ByteString, takeWhile, concat)
import Data.Time
import Data.Text (Text, pack, unpack, intercalate, concat)
import Data.Text.IO (readFile, putStrLn)
import Data.Time
import Data.Word

import Data.Attoparsec.Text hiding (space)
import Data.Attoparsec.Combinator (lookAhead)
import Control.Applicative hiding (empty)


eol = endOfLine
lskip = skipWhile (inClass " :=")
uEOL = lskip *> takeTill isEndOfLine
between open close p = do{ open; x <- p; close; return x }
quoted c = (between (char c) (char c) (takeWhile (/=c)))
squoted = quoted '\''
dquoted = quoted '"'

word = takeWhile1 (/=' ')


parseMemoryMap = do
  "Boundary address"
  return "YEAH"

pdftextParser = parseMemoryMap

fp = "f4reference.txt"

main = do
  f <- readFile fp
  case parseOnly pdftextParser f of
    Left err -> print err
    Right result -> putStrLn $ pack $ show result
