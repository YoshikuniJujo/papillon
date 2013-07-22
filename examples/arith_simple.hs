{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import Prelude hiding (product)
import Text.Papillon
import Data.Char
import Data.Maybe
import System.Environment

main :: IO ()
main = do
	arg : _ <- getArgs
	case expr $ parse arg of
		Right (r, _) -> print r
		Left _ -> putStrLn "parse error"

[papillon|

value :: Int
	= ds:<isDigit>+		{ read ds }
	/ '(' e:expr ')'	{ e }
;
op1 :: Int -> Int -> Int
	= '*'			{ (*) }
	/ '/'			{ div }
;
op2 :: Int -> Int -> Int
	= '+'			{ (+) }
	/ '-'			{ (-) }
;
product :: Int
	= v0:value vs:(op:op1 v:value { (`op` v) })*
				{ foldl (flip ($)) v0 vs }
;
expr :: Int
	= p0:product ps:(op:op2 p:product { (`op` p) })*
				{ foldl (flip ($)) p0 ps }
;
|]
