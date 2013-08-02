{-# LANGUAGE TupleSections #-}

module Text.Papillon.SyntaxTree (
	PegFile,
	PPragma(..),
	ModuleName,
	Exports,
	Code,

	STPeg,
	Peg,
	Definition,
	Selection,
	Expression,
	PlainExpression,
	Check,
	ReadFrom(..),

	Lookahead(..),
	Lists(..),

	pprCheck,
	mkPegFile,
	charList,

	dvCharsN
) where

import Language.Haskell.TH
import Language.Haskell.TH.PprLib
import Control.Arrow ((***))
import Data.List

data Lookahead = Here | Ahead | NAhead String deriving (Show, Eq)
data Lists = List | List1 | Optional deriving (Show, Eq)

type PegFile = ([PPragma], ModuleName, Maybe Exports, Code, STPeg, Code)
data PPragma = LanguagePragma [String] | OtherPragma String deriving Show
type ModuleName = [String]
type Exports = String
type Code = String

type STPeg = (Maybe Type, Type, Peg)
type Peg = [Definition]
type Definition = (String, Maybe Type, Selection)
type Selection =  Either [Expression] [PlainExpression]
type Expression = Either ([(Lookahead, Check)], Exp) Exp
type PlainExpression = [(Lookahead, ReadFrom)]
type Check = ((Pat, String), ReadFrom, Maybe (Exp, String))
data ReadFrom
	= FromVariable (Maybe String)
	| FromSelection Selection
	| FromL Lists ReadFrom
	deriving Show

pprCheck :: Check -> Doc
pprCheck ((pat, _), rf, test) =
	ppr pat <> colon <> ppr rf <> maybe empty (brackets . ppr . fst) test

instance Ppr ReadFrom where
	ppr (FromVariable (Just v)) = text v
	ppr (FromVariable _) = empty
	ppr (FromL l rf) = ppr rf <> ppr l
	ppr (FromSelection sel) = parens $ ps sel
		where
		ps = hsep . intersperse (char '/') . either (map pe) (map ppe)
		pe (Left (ex, hs)) = (<+> braces (ppr hs)) $ hsep $
			map (uncurry ($) . (((<>) . ppr) *** pprCheck)) ex
		pe (Right ex) = char '<' <> ppr ex <> char '>'
		ppe = hsep . map (uncurry (<>) . (ppr *** ppr))


instance Ppr Lookahead where
	ppr Here = empty
	ppr Ahead = char '&'
	ppr (NAhead _) = char '!'

instance Ppr Lists where
	ppr List = char '*'
	ppr List1 = char '+'
	ppr Optional = char '?'

mkPegFile :: [PPragma] -> Maybe ([String], Maybe String) -> String -> String ->
	STPeg -> String -> PegFile
mkPegFile ps (Just md) x y z w = (ps, fst md, snd md, x ++ "\n" ++ y, z, w)
mkPegFile ps Nothing x y z w = (ps, [], Nothing, x ++ "\n" ++ y, z, w)

charList :: String -> ReadFrom
charList cs = FromSelection $ Left $ (: []) $ Right $
	InfixE Nothing (VarE $ mkName "elem") $ Just $ LitE $ StringL cs

dvCharsN :: String
dvCharsN = "char"
