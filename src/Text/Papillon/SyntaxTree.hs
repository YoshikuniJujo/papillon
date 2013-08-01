{-# LANGUAGE TupleSections #-}

module Text.Papillon.SyntaxTree (
	Peg,
	Definition,
	Selection,
	Expression,
	PlainExpression,
	Check,
	ReadFrom(..),

	Q,
	STPegQ,
	PegQ,
	DefinitionQ,
	SelectionQ,
	ExpressionQ,
	PlainExpressionQ,
	CheckQ,
	ReadFromQ,

	stPegQ,
	definitionQ,
	normalSelectionQ,
	plainSelectionQ,
	expressionQ,
	plainExpressionQ,
	check,
	fromSelectionQ,

	Lookahead(..),
	Lists(..),

	fromTokenChars,
	expressionSugar,

	selectionType,
	pprCheck,
	nameFromRF,

	PegFileQ,
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

type STPegQ = STPeg
type PegQ = Peg
type DefinitionQ = Definition
type SelectionQ = Selection
type ExpressionQ = Expression
type PlainExpressionQ = PlainExpression
type CheckQ = Check
type ReadFromQ = ReadFrom

stPegQ :: Maybe Type -> Type -> PegQ -> STPegQ
stPegQ = (,,)

fromSelectionQ :: SelectionQ -> ReadFromQ
fromSelectionQ sel = FromSelection sel

definitionQ :: String -> Maybe Type -> SelectionQ -> DefinitionQ
definitionQ name typq selq = let
	sel = selq
	typ = case typq of
		Just t -> Just t
		_ -> Nothing in
	(name, typ, sel)

normalSelectionQ :: [ExpressionQ] -> SelectionQ
normalSelectionQ expqs = Left expqs

plainSelectionQ :: [PlainExpressionQ] -> SelectionQ
plainSelectionQ expqs = Right expqs

expressionQ :: ([(Lookahead, CheckQ)], Exp) -> ExpressionQ
expressionQ (ls, ex) =
	let	e = ex
		l = map (\(la, c) -> (la ,) c) ls in
		Left (l, e)

plainExpressionQ :: [(Lookahead, ReadFromQ)] -> PlainExpressionQ
plainExpressionQ ls = map (\(la, c) -> (la ,) c) ls

check :: (Pat, String) -> ReadFromQ -> Maybe (Exp, String) -> CheckQ
check (pat, pcom) rfq (Just (test, tcom)) = do
	let	rf = rfq
		p = pat
		t = test in
		((p, pcom), rf, Just (t, tcom))
check (pat, pcom) rfq Nothing = do
	let	rf = rfq
		p = pat in
		((p, pcom), rf, Nothing)

expressionSugar :: Exp -> ExpressionQ
expressionSugar pm = Right pm

fromTokenChars :: String -> ReadFromQ
fromTokenChars cs = do
	let ex = expressionSugar $ InfixE Nothing (VarE $ mkName "elem") $
		Just $ LitE $ StringL cs
	FromSelection $ Left [ex]

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
selectionType peg tk e = do
	case e of
		Right ex -> foldr (\x y -> (eitherT `AppT` x) `AppT` y)
			(last $ types ex) (init $ types ex)
		Left [Left ex] | tc ex -> tk
		Left [Right _] -> tk
		_ -> error "selectionType: can't get type"
	where
	eitherT = ConT $ mkName "Either"
	types e' = map (plainExpressionType peg tk) e'
	tc ([(Here, ((VarP p, _), FromVariable Nothing, _))], VarE v) = p == v
	tc _ = False

plainExpressionType :: Peg -> Type -> PlainExpression -> Type
plainExpressionType peg tk e = let fe = filter ((== Here) . fst) e in
	foldl AppT (TupleT $ length fe) $ map (readFromType peg tk . snd) $ fe

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

nameFromSelection :: Selection -> [String]
nameFromSelection exs = concat $
	(either (mapM nameFromExpression) (mapM nameFromPlainExpression) exs)

nameFromExpression :: Expression -> [String]
nameFromExpression (Left e) = nameFromCheck $ snd $ head $ fst e
nameFromExpression (Right _) = [dvCharsN]

nameFromPlainExpression :: PlainExpression -> [String]
nameFromPlainExpression = concat . map (nameFromRF . snd)

nameFromCheck :: Check -> [String]
nameFromCheck (_, rf, _) = nameFromRF rf

nameFromRF :: ReadFrom -> [String]
nameFromRF (FromVariable (Just s)) = [s]
nameFromRF (FromVariable _) = [dvCharsN]
nameFromRF (FromL _ rf) = nameFromRF rf
nameFromRF (FromSelection sel) = nameFromSelection sel

type PegFile = ([PPragma], ModuleName, Maybe Exports, Code, STPeg, Code)
type PegFileQ = Q PegFile
data PPragma = LanguagePragma [String] | OtherPragma String deriving Show
type ModuleName = [String]
type Exports = String
type Code = String

mkPegFile :: [PPragma] -> Maybe ([String], Maybe String) -> String -> String ->
	STPegQ -> String -> PegFileQ
mkPegFile ps (Just md) x y zq w = do
	let z = zq
	return (ps, fst md, snd md, x ++ "\n" ++ y, z, w)
mkPegFile ps Nothing x y zq w = do
	let z = zq
	return (ps, [], Nothing, x ++ "\n" ++ y, z, w)
