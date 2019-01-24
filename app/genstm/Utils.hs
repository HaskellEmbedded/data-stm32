module Utils where

import Turtle
import qualified Turtle as TU
import qualified Data.Text as T
import qualified System.IO as SIO

fpToText = format fp
fpToString = T.unpack . format fp

fpToGHCfp :: TU.FilePath -> SIO.FilePath
fpToGHCfp = fromString . fpToString

tshow :: Show a => a -> T.Text
tshow = T.pack . show

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g a b = f (g a) (g b)

replaceOne :: Text -> Text -> Text -> Text
replaceOne pattern substitution text
  | T.null back = text    -- pattern doesn't occur
  | otherwise = T.concat [front, substitution, T.drop (T.length pattern) back] 
    where
      (front, back) = T.breakOn pattern text
