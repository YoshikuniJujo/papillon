{-# LANGUAGE TemplateHaskell #-}

module TestName (
	some
) where

import Language.Haskell.TH
import Data.Char

some :: Char -> ExpQ
some c = varE 'isSpace `appE` litE (charL c)

some' :: Char -> ExpQ
some' c = varE (mkName "isSpace") `appE` litE (charL c)
