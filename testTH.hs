{-# LANGUAGE QuasiQuotes, PackageImports #-}

module Main where

import Text.Papillon
import Data.Char

main :: IO ()
main = do
	case dv_some $ parse "c3" of
		Just (r, _d) -> print r
		_ -> putStrLn "bad"

[papillon|
other :: Char
	= d:[isDigit]	{ d }
;
some :: Char
	= d:[isDigit] l:[isLower]	{ d }
	/ l:[isLower] d:[isDigit]	{ l }
;
|]
