module Data where

import Language.Haskell.TH

getGetter :: Int -> ExpQ
getGetter 1 = varE $ mkName "str"
getGetter 2 = varE $ mkName "int"
getGetter 3 = varE $ mkName "chr"
