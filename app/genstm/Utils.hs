module Utils where

import Turtle
import qualified Turtle as TU
import qualified Data.Text as T
import qualified System.IO as SIO
import Data.Char (toLower)
import Data.Algorithm.Diff
import Control.Monad

cdmk dir = do
  hasdir <- testdir dir
  when (not hasdir) (mktree dir)
  cd dir

fpToText = format fp
fpToString = T.unpack . format fp

fpToGHCfp :: TU.FilePath -> SIO.FilePath
fpToGHCfp = fromString . fpToString

tshow :: Show a => a -> T.Text
tshow = T.pack . show

assert :: MonadIO f => Text -> Bool -> f ()
assert msg cond = unless cond $ die msg

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g a b = f (g a) (g b)

replaceOne :: Text -> Text -> Text -> Text
replaceOne pattern substitution text
  | T.null back = text    -- pattern doesn't occur
  | otherwise = T.concat [front, substitution, T.drop (T.length pattern) back] 
    where
      (front, back) = T.breakOn pattern text

lower :: String -> String
lower = map toLower

renderDiff :: (Foldable t, Show a) => t (Diff a) -> String
renderDiff x = concatMap renderDiffVal x

renderDiffVal :: Show a => Diff a -> String
renderDiffVal (Both a _a) = "   " ++ show a ++ "\n"
renderDiffVal (First a)   = " - " ++ show a ++ "\n"
renderDiffVal (Second a)  = " + " ++ show a ++ "\n"
