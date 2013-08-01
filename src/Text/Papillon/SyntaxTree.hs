{-# LANGUAGE TupleSections #-}

module Text.Papillon.SyntaxTree (
	STPeg,
	Peg,
	Definition,
	Selection,
	Expression,
	PlainExpression,
	Check,
	ReadFrom(..),
	charList,

	Lookahead(..),
	Lists(..),

	selectionType,
	pprCheck,
	readings,

	PegFile,
	mkPegFile,
	PPragma(..),
	ModuleName,
	Exports,
	Code,

	dvCharsN
) where

import Language.Haskell.TH
import Language.Haskell.TH.PprLib
import Control.Arrow ((***))
import Data.List

dvCharsN :: String
dvCharsN = "char"

data Lookahead = Here | Ahead | NAhead String deriving (Show, Eq)
data Lists = List | List1 | Optional deriving (Show, Eq)

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

charList :: String -> ReadFrom
charList cs = FromSelection $ Left $ (: []) $ Right $
	InfixE Nothing (VarE $ mkName "elem") $ Just $ LitE $ StringL cs

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

definitionType :: Peg -> Type -> Definition -> Type
definitionType _ _ (_, Just typ, _) = typ
definitionType peg tk (_, _, sel) = selectionType peg tk sel

selectionType :: Peg -> Type -> Selection -> Type
selectionType peg tk e = case e of
	Right ex -> foldr (\x y -> (eitherT `AppT` x) `AppT` y)
		(last $ types ex) (init $ types ex)
	Left [Left ex] | tc ex -> tk
	Left [Right _] -> tk
	_ -> error "selectionType: can't get type"
	where
	eitherT = ConT $ mkName "Either"
	types = map $ plainExpressionType peg tk
	tc ([(Here, ((VarP p, _), FromVariable Nothing, _))], VarE v) = p == v
	tc _ = False

plainExpressionType :: Peg -> Type -> PlainExpression -> Type
plainExpressionType peg tk e = let fe = filter ((== Here) . fst) e in
	foldl AppT (TupleT $ length fe) $ map (readFromType peg tk . snd) fe

readFromType :: Peg -> Type -> ReadFrom -> Type
readFromType peg tk (FromVariable (Just v)) =
	definitionType peg tk $ searchDefinition peg v
readFromType peg tk (FromSelection sel) = selectionType peg tk sel
readFromType _ tk (FromVariable _) = tk
readFromType peg tk (FromL l rf) = lt l `AppT` readFromType peg tk rf
	where	lt Optional = ConT $ mkName "Maybe"
		lt _ = ListT

searchDefinition :: Peg -> String -> Definition
searchDefinition peg name = case flip filter peg $ (== name) . \(n, _, _) -> n of
	[d] -> d
	_ -> error "searchDefinitionQ: bad"

readings :: ReadFrom -> [String]
readings (FromVariable (Just s)) = [s]
readings (FromVariable _) = [dvCharsN]
readings (FromL _ rf) = readings rf
readings (FromSelection s) = concat $ either
	(mapM $ either
		(readings . (\(_, rf, _) -> rf) . snd . head . fst)
		(const [dvCharsN]))
	(mapM $ concatMap $ readings . snd) s

type PegFile = ([PPragma], ModuleName, Maybe Exports, Code, STPeg, Code)
data PPragma = LanguagePragma [String] | OtherPragma String deriving Show
type ModuleName = [String]
type Exports = String
type Code = String

mkPegFile :: [PPragma] -> Maybe ([String], Maybe String) -> String -> String ->
	STPeg -> String -> PegFile
mkPegFile ps (Just md) x y z w = (ps, fst md, snd md, x ++ "\n" ++ y, z, w)
mkPegFile ps Nothing x y z w = (ps, [], Nothing, x ++ "\n" ++ y, z, w)
