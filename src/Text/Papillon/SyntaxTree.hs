module Text.Papillon.SyntaxTree where

import Language.Haskell.TH
import Data.Char

type MaybeString = Maybe String

type Nil = ()
-- type Leaf = Either String ExR

type Leaf = (Maybe String, ExR)
data NameLeaf
	= NameLeaf PatQ Leaf
	| NameLeafList PatQ Selection
data NameLeaf_ = NotAfter NameLeaf | Here NameLeaf
notAfter, here :: NameLeaf -> NameLeaf_
notAfter = NotAfter
here = Here
type Expression = [NameLeaf_]
type ExpressionHs = (Expression, ExR)
type Selection = [ExpressionHs]
type Typ = Name
type Definition = (String, Typ, Selection)
type Peg = [Definition]
type TTPeg = (TypeQ, TypeQ, Peg)

type Ex = (ExpQ -> ExpQ) -> ExpQ
type ExR = ExpQ

ctLeaf :: Leaf
-- ctLeaf = Right $ conE (mkName "True") -- varE (mkName "const") `appE` conE (mkName "True")
ctLeaf = (Nothing, conE $ mkName "True")

ruleLeaf :: String -> ExpQ -> Leaf
boolLeaf :: ExpQ -> Leaf
ruleLeaf r t = (Just r, t) -- Left
boolLeaf p = (Nothing, p) -- Right

true :: ExpQ
true = conE $ mkName "True"

just :: a -> Maybe a
just = Just
nothing :: Maybe a
nothing = Nothing

nil :: Nil
nil = ()

cons :: a -> [a] -> [a]
cons = (:)

type PatQs = [PatQ]

mkNameLeaf :: PatQ -> Leaf -> NameLeaf
mkNameLeaf = NameLeaf

mkNameLeafList :: PatQ -> Selection -> NameLeaf
mkNameLeafList = NameLeafList

strToPatQ :: String -> PatQ
strToPatQ = varP . mkName

conToPatQ :: String -> [PatQ] -> PatQ
conToPatQ t ps = conP (mkName t) ps

mkExpressionHs :: a -> Ex -> (a, ExR)
mkExpressionHs x y = (x, getEx y)

mkDef :: a -> String -> c -> (a, Name, c)
mkDef x y z = (x, mkName y, z)

toExp :: String -> Ex
-- toExp v = \f -> f `appE` varE (mkName v)
toExp v = \f -> f $ varE (mkName v)

toEx :: ExR -> Ex
toEx v = \f -> f v

apply :: String -> Ex -> Ex
apply f x = \g -> x (toExp f g `appE`)

getEx :: Ex -> ExR
getEx ex = ex id -- (varE $ mkName "id")

toExGetEx :: Ex -> Ex
toExGetEx = toEx . getEx

emp :: [a]
emp = []

type PegFile = (String, String, TTPeg, String)
mkPegFile :: Maybe String -> Maybe String -> String -> String -> TTPeg -> String
	-> PegFile
mkPegFile (Just p) (Just md) x y z w =
	("{-#" ++ p ++ addPragmas ++ "module " ++ md ++ " where\n" ++
	addModules,
	x ++ "\n" ++ y, z, w)
mkPegFile Nothing (Just md) x y z w =
	(x ++ "\n" ++ "module " ++ md ++ " where\n" ++
	addModules,
	x ++ "\n" ++ y, z, w)
mkPegFile (Just p) Nothing x y z w = (
	"{-#" ++ p ++ addPragmas ++
	addModules,
	x ++ "\n" ++ y
	, z, w)
mkPegFile Nothing Nothing x y z w = (addModules, x ++ "\n" ++ y, z, w)

addPragmas, addModules :: String
addPragmas =
	", FlexibleContexts, PackageImports, TypeFamilies, RankNTypes, " ++
	"FlexibleInstances #-}\n"
addModules =
	"import \"monads-tf\" Control.Monad.State\n" ++
	"import \"monads-tf\" Control.Monad.Error\n" ++
	"import Control.Monad.Trans.Error (Error (..))\n"

charP :: Char -> PatQ
charP = litP . charL
stringP :: String -> PatQ
stringP = litP . stringL

isAlphaNumOt, elemNTs :: Char -> Bool
isAlphaNumOt c = isAlphaNum c || c `elem` "{-#.\":}|[]!;=/ *()"
elemNTs = (`elem` "nt\\'")

isKome, isOpen, isClose :: Char -> Bool
isKome = (== '*')
isOpen = (== '(')
isClose = (== ')')

getNTs :: Char -> Char
getNTs 'n' = '\n'
getNTs 't' = '\t'
getNTs '\\' = '\\'
getNTs '\'' = '\''
getNTs o = o
isLowerU :: Char -> Bool
isLowerU c = isLower c || c == '_'

tString :: String
tString = "String"
mkTTPeg :: String -> Peg -> TTPeg
mkTTPeg s p =
	(conT $ mkName s, conT (mkName "Token") `appT` conT (mkName s), p)
