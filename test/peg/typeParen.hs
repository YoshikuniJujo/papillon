{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import Text.Papillon
import Data.Char

main :: IO ()
main = do
	case sub $ parse "8-3-3-1" of
		Right (r, _) -> print (r id)
		Left _ -> putStrLn "parse error"

type Hoge = (Int -> Int) -> Int

[papillon|

num :: Int = ds:<isDigit>+		{ read ds };

sub :: (Int -> Int) -> Int
	= n:num '-' s:sub		{ \f -> s (f n -) }
	/ n:num				{ \f -> f n }
;
|]
