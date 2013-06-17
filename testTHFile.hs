{-# LANGUAGE TemplateHaskell #-}

import Text.Papillon

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Char

type Nil = ()
type Strings = [String]
type Leaf = Either [String] String

left = Left
right = Right

nil :: Nil
nil = ()

cons :: a -> [a] -> [a]
cons = (:)

empty :: [a]
empty = []

isOpenBr, isCloseBr, isSymbolOne, isSymbolTwo :: Char -> Bool
isOpenBr = (== '[')
isCloseBr = (== ']')
isSymbolOne = (`elem` "=/;+*() ")
isSymbolTwo = (`elem` "\\'nt")

do	cnt <- runIO $ readFile "test.peg"
	quoteDec papillon cnt
	
main :: IO ()
main = do
	case dv_test $ parse "[aAdxy hoge]Z=' 3" of
		Just (r, _d) -> print r
		_ -> putStrLn "bad"
	debug
