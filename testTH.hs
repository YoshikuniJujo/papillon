{-# LANGUAGE QuasiQuotes, PackageImports #-}

module Main where

import Text.Papillon
import "monads-tf" Control.Monad.State
import Data.Char

main :: IO ()
main = do
	case dvSome $ parse "3" of
		Just (r, _d) -> print r
		_ -> putStrLn "bad"
	debug

[papillon|

some :: Char
	= d:[isDigit]	{ d }

|]
