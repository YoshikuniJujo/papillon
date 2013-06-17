{-# LANGUAGE QuasiQuotes #-}

module Main where

import Text.Papillon

main :: IO ()
main = case dvSome $ parse "3" of
	Just (r, _d) -> print r
	_ -> error "bad"

[papillon|

some :: Char
	= d:[isDigit]	{ d }

|]
