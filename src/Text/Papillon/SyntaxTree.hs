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
	showCheck,
	nameFromRF,

	PegFileQ,
	mkPegFile,
	PPragma(..),
	ModuleName,
	Exports,
	Code
) where

import Language.Haskell.TH
import Control.Monad
import Control.Applicative
import Control.Arrow
import Data.List
import Data.IORef

data Lookahead = Here | Ahead | NAhead String deriving (Show, Eq)
data Lists = List | List1 | Optional deriving Show

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

showSelection :: Selection -> String
showSelection =
	intercalate " / " . either (map showExpression) (map showPlainExpression)

showExpression :: Expression -> String
showExpression exhs = let (ex, hs) = exhs in
	(\e -> unwords e ++ " { " ++ show (ppr hs) ++ " }")
		$ map (uncurry ($) . (((++) . showLA) *** showCheck)) ex

showPlainExpression :: PlainExpression -> String
showPlainExpression rfs =
	unwords $ map (\(ha, rf) -> (showLA ha ++) $ showReadFrom rf) rfs

showLA :: Lookahead -> String
showLA Here = ""
showLA Ahead = "&"
showLA (NAhead _) = "!"

showCheck :: Check -> String
showCheck ((pat, _), rf, Just (p, _)) = let rff = showReadFrom rf in
	show (ppr pat) ++ ":" ++ rff ++ "[" ++ show (ppr p) ++ "]"
showCheck ((pat, _), rf, Nothing) = let rff = showReadFrom rf in
	show (ppr pat) ++ ":" ++ rff

showReadFrom :: ReadFrom -> String
showReadFrom (FromVariable (Just v)) = v
showReadFrom (FromVariable _) = ""
showReadFrom (FromL l rf) = (++ sl l) $ showReadFrom rf
	where	sl List = "*"
		sl List1 = "+"
		sl Optional = "?"
showReadFrom (FromSelection sel) = ('(' :) $ (++ ")") $ showSelection sel

definitionType :: PegQ -> TypeQ -> DefinitionQ -> TypeQ
definitionType pegq tk defq = do
	def <- defq =<< runIO (newIORef 0)
	case def of
		(_, Just typ, _) -> return typ
		(_, _, sel) -> selectionType pegq tk sel

selectionType :: PegQ -> TypeQ -> Selection -> TypeQ
selectionType peg tk e = do
	case e of
		Right ex -> foldr (\x y -> (eitherT `appT` x) `appT` y)
			(last $ types ex) (init $ types ex)
		Left [ex] | isTypeChar ex -> tk
		_ -> error "selectionType: can't get type"
	where
	eitherT = conT $ mkName "Either"
	types e' = map (plainExpressionType peg tk) e'

isTypeChar :: Expression -> Bool
isTypeChar ([(Here, ((VarP p, _), FromVariable Nothing, _))], VarE v) = p == v
isTypeChar _ = False

plainExpressionType :: PegQ -> TypeQ -> PlainExpression -> TypeQ
plainExpressionType peg tk e = let fe = filter ((== Here) . fst) e in
	foldl appT (tupleT $ length fe) $ map (readFromType peg tk . snd) $ fe

readFromType :: PegQ -> TypeQ -> ReadFrom -> TypeQ
readFromType peg tk (FromVariable (Just v)) =
	definitionType peg tk $ searchDefinition peg v
readFromType pegq tk (FromSelection sel) = selectionType pegq tk sel
readFromType _ tk (FromVariable _) = tk
readFromType peg tk (FromL l rf) = lt l `appT` readFromType peg tk rf
	where	lt Optional = conT $ mkName "Maybe"
		lt _ = listT

searchDefinition :: PegQ -> String -> DefinitionQ
searchDefinition pegq name = \g -> do
	peg <- pegq g
	let ds = flip filter peg $ (== name) . (\(n, _, _) -> n)
	return $ case ds of
		[d] -> d
		_ -> error "searchDefinition: bad"

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
