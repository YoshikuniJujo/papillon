{-# LANGUAGE TemplateHaskell #-}

module MkParser (
	dv_peg,
	parse
) where

import Text.Papillon

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Char

type Nil = ()
data Leaf_ = NotAfter Leaf | Here Leaf
notAfter, here :: Leaf -> Leaf_
notAfter = NotAfter
here = Here
type Leaf = Either ExR String
type NameLeaf = (Name, Leaf_)
type Expression = [NameLeaf]
type ExpressionHs = (Expression, ExR)
type Selection = [ExpressionHs]
type Typ = Name
type Definition = (String, Typ, Selection)
type Peg = [Definition]

type Ex = ExpQ -> ExpQ
type ExR = ExpQ

left :: a -> Either a b
right :: b -> Either a b
left = Left
right = Right

nil :: Nil
nil = ()

cons :: a -> [a] -> [a]
cons = (:)
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

isOpenBr, isCloseBr, isEqual, isSlash, isSemi,
	isColon, isOpenWave, isCloseWave, isLowerU, isNot :: Char -> Bool
isOpenBr = (== '[')
isCloseBr = (== ']')
isEqual = (== '=')
isSlash = (== '/')
isSemi = (== ';')
isColon = (== ':')
isOpenWave = (== '{')
isCloseWave = (== '}')
isLowerU c = isLower c || c == '_'
isNot = (== '!')

do	cnt <- runIO $ readFile "test.peg"
	quoteDec papillon cnt
