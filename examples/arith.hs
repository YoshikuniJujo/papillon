{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import Text.Papillon
import Data.Char
import System.Environment

main :: IO ()
main = do
	arg : _ <- getArgs
	case expr $ parse arg of
		Right (r, _) -> print r
		Left _ -> putStrLn "parse error"

[papillon|

op1 :: Int -> Int -> Int
	= '*'			{ (*) }
	/ '/'			{ div }
	/ '%'			{ mod }
;
op2 :: Int -> Int -> Int
	= '+'			{ (+) }
	/ '-'			{ (-) }
;
factor :: Int
	= ds:<isDigit>+		{ read ds }
	/ '(' e:expr ')'	{ e }
;
term :: Int
	= v0:factor vs:(op:op1 v:factor { (`op` v) })*
				{ foldl (flip ($)) v0 vs }
;
expr :: Int
	= p0:term ps:(op:op2 p:term { (`op` p) })*
				{ foldl (flip ($)) p0 ps }
;
|]
