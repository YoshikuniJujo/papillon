{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import Text.Papillon
import System.Environment
import Data.Char

main :: IO ()
main = do
	fp : _ <- getArgs
	cnt <- readFile fp
	case conf $ parse cnt of
		Right (r, _) -> print r
		Left _ -> putStrLn "parse error"

[papillon|

name :: String
	= s:(c:[isLower c] { c })+		{ s }
;
value :: Int
	= ds:(d:[isDigit d] { d })+		{ read ds }
;
conf1 :: (String, Int)
	= n:name ':' ' ' v:value		{ (n, v) }
;
conf :: [(String, Int)]
	= cs:(c:conf1 '\n' { c })+ !_:[True]	{ cs }
;

|]
