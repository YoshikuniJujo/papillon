{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import Text.Papillon
import Data.Char

main :: IO ()
main = case runError $ var $ parse "hello, world" of
	Right (r, _) -> print r
	Left _ -> putStrLn "parse error"

[papillon|

var :: String = cs:<isLower>+ ',' { cs };

|]
