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
	Code
) where

import Language.Haskell.TH
import Language.Haskell.TH.PprLib
import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Data.List
import Data.IORef

data Lookahead = Here | Ahead | NAhead String deriving (Show, Eq)
data Lists = List | List1 | Optional deriving (Show, Eq)

type STPeg = (Type, Peg)
type Peg = [Definition]
type Definition = (String, Maybe Type, Selection)
type Selection =  Either [Expression] [PlainExpression]
type Expression = ([(Lookahead, Check)], Exp)
type PlainExpression = [(Lookahead, ReadFrom)]
type Check = ((Pat, String), ReadFrom, Maybe (Exp, String))
data ReadFrom
	= FromVariable (Maybe String)
	| FromSelection Selection
	| FromL Lists ReadFrom
	deriving Show

type STPegQ = IORef Int -> Q STPeg
type PegQ = IORef Int -> Q Peg
type DefinitionQ = IORef Int -> Q Definition
type SelectionQ = IORef Int -> Q Selection
type ExpressionQ = IORef Int -> Q Expression
type PlainExpressionQ = IORef Int -> Q PlainExpression
type CheckQ = IORef Int -> Q Check
type ReadFromQ = IORef Int -> Q ReadFrom

stPegQ :: TypeQ -> PegQ -> STPegQ
stPegQ stq pegq = ((,) <$> stq <*>) . pegq

fromSelectionQ :: SelectionQ -> ReadFromQ
fromSelectionQ sel = (FromSelection <$>) . sel

definitionQ :: String -> Maybe TypeQ -> SelectionQ -> DefinitionQ
definitionQ name typq selq = \g -> do
	sel <- selq g
	typ <- case typq of
		Just t -> Just <$> t
		_ -> return Nothing
	return $ (name, typ, sel)

normalSelectionQ :: [ExpressionQ] -> SelectionQ
normalSelectionQ expqs = \g -> (Left <$>) $ forM expqs ($ g)

plainSelectionQ :: [PlainExpressionQ] -> SelectionQ
plainSelectionQ expqs = \g -> (Right <$>) $ forM expqs ($ g)

expressionQ :: ([(Lookahead, CheckQ)], ExpQ) -> ExpressionQ
expressionQ (ls, ex) g = do
	e <- ex
	l <- mapM (\(la, c) -> (la ,) <$> c g) ls
	return (l, e)

plainExpressionQ :: [(Lookahead, ReadFromQ)] -> PlainExpressionQ
plainExpressionQ ls g = do
	l <- mapM (\(la, c) -> (la ,) <$> c g) ls
	return l

check :: (PatQ, String) -> ReadFromQ -> Maybe (ExpQ, String) -> CheckQ
check (pat, pcom) rfq (Just (test, tcom)) g = do
	rf <- rfq g
	p <- pat
	t <- test
	return $ ((p, pcom), rf, Just (t, tcom))
check (pat, pcom) rfq Nothing g = do
	rf <- rfq g
	p <- pat
	return $ ((p, pcom), rf, Nothing)

expressionSugar :: ExpQ -> ExpressionQ
expressionSugar pm g = do
	p <- pm
	s <- runIO $ readIORef g
	c <- newName $ "c" ++ show s
	return $ (, VarE c) $ (: []) $ (Here ,) $ (,,)
		(VarP c, "") (FromVariable Nothing) (Just $ (, "") $ p `AppE` VarE c)

fromTokenChars :: String -> ReadFromQ
fromTokenChars cs = \g -> do
	ex <- flip expressionSugar g $ infixE Nothing (varE $ mkName "elem") $
		Just $ litE $ stringL cs
	return $ FromSelection $ Left [ex]

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
		pe (ex, hs) = (<+> braces (ppr hs)) $ hsep $
			map (uncurry ($) . (((<>) . ppr) *** pprCheck)) ex
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
		Left [ex] | tc ex -> tk
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

nameFromSelection :: Selection -> Q [String]
nameFromSelection exs = concat <$>
	(either (mapM nameFromExpression) (mapM nameFromPlainExpression) exs)

nameFromExpression :: Expression -> Q [String]
nameFromExpression = nameFromCheck . snd . head . fst

nameFromPlainExpression :: PlainExpression -> Q [String]
nameFromPlainExpression = (concat <$>) . mapM (nameFromRF . snd)

nameFromCheck :: Check -> Q [String]
nameFromCheck (_, rf, _) = nameFromRF rf

nameFromRF :: ReadFrom -> Q [String]
nameFromRF (FromVariable (Just s)) = return [s]
nameFromRF (FromVariable _) = return ["char"]
nameFromRF (FromL _ rf) = nameFromRF rf
nameFromRF (FromSelection sel) = nameFromSelection sel

type PegFile = ([PPragma], ModuleName, Maybe Exports, Code, STPeg, Code)
type PegFileQ = IORef Int -> Q PegFile
data PPragma = LanguagePragma [String] | OtherPragma String deriving Show
type ModuleName = [String]
type Exports = String
type Code = String

mkPegFile :: [PPragma] -> Maybe ([String], Maybe String) -> String -> String ->
	STPegQ -> String -> PegFileQ
mkPegFile ps (Just md) x y zq w g = do
	z <- zq g
	return (ps, fst md, snd md, x ++ "\n" ++ y, z, w)
mkPegFile ps Nothing x y zq w g = do
	z <- zq g
	return (ps, [], Nothing, x ++ "\n" ++ y, z, w)
