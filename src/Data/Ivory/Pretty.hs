{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Ivory.Pretty where

import Data.List
import Data.List.Split (splitOn)
import Text.Printf
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle)

import qualified Data.Text
import qualified Prettyprinter.Render.Terminal

folddoc
  :: (Doc ann -> Doc ann -> Doc ann)
  -> [Doc ann]
  -> Doc ann
folddoc _ []     = mempty
folddoc _ [x]    = x
folddoc f (x:xs) = f x (folddoc f xs)

encloseStack
  :: Doc ann
  -> Doc ann
  -> Doc ann
  -> [Doc ann]
  -> Doc ann
encloseStack l r p ds = case ds of
  [] -> mempty
  [d] -> l <+> d <> line <> r
  _ -> align
    (  l
    <+> (folddoc (\a b -> a <> line <> p <+> b) ds)
    <> line
    <> r
    )

replace old new = intercalate new . splitOn old

comment x =
      pretty ("--" :: String)
  <+> x
  <>  hardline

rpad m xs | m <= length xs = xs
rpad m xs | otherwise = take m $ xs ++ repeat ' '

displayIvory :: Doc AnsiStyle -> String
displayIvory =
    Data.Text.unpack
  . Prettyprinter.Render.Terminal.renderStrict
  . layoutPretty defaultLayoutOptions

-- preserve indent but no colors
displayIvoryCompact :: Doc AnsiStyle -> String
displayIvoryCompact = displayIvory . unAnnotate
