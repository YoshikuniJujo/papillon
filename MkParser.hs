{-# LANGUAGE TemplateHaskell, PackageImports #-}

module MkParser (
	dv_peg,
	parse
) where

import Text.Papillon

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Char
import Control.Monad
import "monads-tf" Control.Monad.State

type Nil = ()
type Strings = [String]
type Leaf = Either ExR String
type NameLeaf = (Name, Leaf)
type Expression = [NameLeaf]
type ExpressionHs = (Expression, ExR)
type Selection = [ExpressionHs]
type Typ = Name
type Definition = (String, Typ, Selection)
type Peg = [Definition]

type Ex = ExpQ -> ExpQ
type ExR = ExpQ

left = Left
right = Right

nil :: Nil
nil = ()

cons :: a -> [a] -> [a]
cons = (:)
tuple :: a -> b -> (a, b)
tuple = (,)
tupleThree :: a -> b -> c -> (a, b, c)
tupleThree = (,,)
mkNameLeaf :: String -> b -> (Name, b)
mkNameLeaf x y = (mkName x, y)
mkExpressionHs :: a -> Ex -> (a, ExR)
mkExpressionHs x y = (x, getEx y)
mkDef :: a -> String -> c -> (a, Name, c)
mkDef x y z = (x, mkName y, z)

toExp :: String -> Ex
toExp v = \f -> f `appE` varE (mkName v)

apply :: String -> Ex -> Ex
apply f x = \g -> x (toExp f g)

getEx :: Ex -> ExR
getEx ex = ex (varE $ mkName "id")

empty :: [a]
empty = []

isOpenBr, isCloseBr, isSymbolOne, isSymbolTwo, isEqual, isSlash :: Char -> Bool
isOpenBr = (== '[')
isCloseBr = (== ']')
isSymbolOne = (`elem` "=/;+*() ")
isSymbolTwo = (`elem` "\\'nt")
isEqual = (== '=')
isSlash = (== '/')
isSemi = (== ';')
isColon = (== ':')
isOpenWave = (== '{')
isCloseWave = (== '}')

do	cnt <- runIO $ readFile "test.peg"
	quoteDec papillon cnt
	
main :: IO ()
main = do
	case dv_peg $ parse $ "heko = h:hage p:[aAdxy hoge]h:hige { hoge }/" ++
			"p:posoZ{ boka };hage=bo { boke };' 3" of
		Just (r@((n, _, _) : _), _d) -> print n
		_ -> putStrLn "bad"
	debug
