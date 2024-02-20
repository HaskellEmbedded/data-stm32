{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Ivory.Pretty where

import Data.List
import Data.List.Split (splitOn)
import Text.Printf
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

folddoc :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
folddoc _ []     = empty
folddoc _ [x]    = x
folddoc f (x:xs) = f x (folddoc f xs)

encloseStack :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseStack l r p ds = case ds of
  [] -> empty -- l </> r
  [d] -> l <+> d </> r
  _ -> align (l <+> (folddoc (\a b -> a </> p <+> b) ds) </> r)

replace old new = intercalate new . splitOn old

comment x = string "--" <+> x <> hardline

hexFormat :: Int -> String
hexFormat = printf "0x%x"
ppHex = text . hexFormat

rpad m xs | m <= length xs = xs
rpad m xs | otherwise = take m $ xs ++ repeat ' '

displayIvory pp = displayS (renderPretty 0 1000 pp) ""
-- preserve indent but no colors
displayIvoryCompact pp = displayS' (renderPretty 0 1000 pp) ""

-- SSGR ignoring version of displayS
displayS' :: SimpleDoc -> ShowS
displayS' SFail              = error $ "@SFail@ can not appear uncaught in a " ++
                              "rendered @SimpleDoc@"
displayS' SEmpty             = id
displayS' (SChar c x)        = showChar c . displayS' x
displayS' (SText l s x)      = showString s . displayS' x
displayS' (SLine i x)        = showString ('\n':spaces i) . displayS' x
  where
  spaces :: Int -> String
  spaces n | n <= 0    = ""
           | otherwise = replicate n ' '
displayS' (SSGR _s x)         = displayS' x
