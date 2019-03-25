
module Data.CMX.Parse.Util where

import Text.XML.HXT.Core
import Data.Maybe
import qualified Data.Char as Char

capitalized :: String -> String
capitalized (head:tail) = Char.toUpper head : map Char.toLower tail
capitalized [] = []

atTag tag = deep (isElem >>> hasName tag)


-- nonempty attr value
attNE x = (getAttrValue x >>> isA (/= ""))

attMaybe attname tagname = withDefault (arr Just <<< attNE attname <<< atTag tagname) Nothing

