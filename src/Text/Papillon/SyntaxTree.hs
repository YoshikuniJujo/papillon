{-# LANGUAGE TupleSections #-}

module Text.Papillon.SyntaxTree (
	Peg,
	Definition,
	Selection,
	Expression,
	PlainExpression,
	Check,
	ReadFrom(..),

	ExpressionQ,
	CheckQ,

	expressionQ,
	check,

	Lookahead(..),
	Lists(..),

	fromTokenChars,
	expressionSugar,

	selectionType,
	showCheck,
	showCheckQ,
	nameFromRF,

	PegFile,
	mkPegFile,
	PPragma(..),
	ModuleName,
	Exports,
	Code
) where

import Language.Haskell.TH
import Control.Applicative
import Control.Arrow
import Data.List

data Lookahead = Here | Ahead | NAhead String deriving Eq
data Lists = List | List1 | Optional deriving Show

type Peg = [Definition]
type Definition = (String, Maybe TypeQ, Selection)
type Selection =  Either [ExpressionQ] [PlainExpression]
type Expression = (Bool, ([(Lookahead, Check)], Exp))
type PlainExpression = [(Lookahead, ReadFrom)]
type Check = ((Pat, String), ReadFrom, Maybe (Exp, String))
data ReadFrom
	= FromVariable (Maybe String)
	| FromSelection Selection
	| FromL Lists ReadFrom

type ExpressionQ = Name -> Q Expression
type CheckQ = Q Check

expressionQ :: Bool -> ([(Lookahead, CheckQ)], ExpQ) -> ExpressionQ
expressionQ b (ls, ex) = const $ do
	e <- ex
	l <- mapM (\(la, c) -> (la ,) <$> c) ls
	return (b, (l, e))

check :: (PatQ, String) -> ReadFrom -> Maybe (ExpQ, String) -> CheckQ
check (pat, pcom) rf (Just (test, tcom)) = do
	p <- pat
	t <- test
	return $ ((p, pcom), rf, Just (t, tcom))
check (pat, pcom) rf Nothing = do
	p <- pat
	return $ ((p, pcom), rf, Nothing)

expressionSugar :: ExpQ -> ExpressionQ
expressionSugar pm c = do
	p <- pm
	return $ (True ,) $ (, VarE c) $ (: []) $ (Here ,) $ (,,)
		(VarP c, "") (FromVariable Nothing) (Just $ (, "") $ p `AppE` VarE c)

fromTokenChars :: String -> ReadFrom
fromTokenChars cs = FromSelection $ Left $ (: []) $ expressionSugar $
	infixE Nothing (varE $ mkName "elem") $ Just $ litE $ stringL cs

showSelection :: Selection -> Q String
showSelection ehss = intercalate " / " <$>
	either (mapM showExpressionQ) (mapM showPlainExpression) ehss

showExpressionQ :: ExpressionQ -> Q String
showExpressionQ e = e (mkName "c") >>= showExpression

showExpression :: Expression -> Q String
showExpression (_, exhs) = let (ex, hs) = exhs in
	(\e -> unwords e ++ " { " ++ show (ppr hs) ++ " }")
		<$> mapM (uncurry (<$>) . (((++) . showLA) *** showCheck)) ex
--		<*> hs

showPlainExpression :: PlainExpression -> Q String
showPlainExpression rfs =
	unwords <$> mapM (\(ha, rf) -> (showLA ha ++) <$> showReadFrom rf) rfs

showLA :: Lookahead -> String
showLA Here = ""
showLA Ahead = "&"
showLA (NAhead _) = "!"

showCheckQ :: CheckQ -> Q String
showCheckQ = (>>= showCheck)

showCheck :: Check -> Q String
showCheck ((pat, _), rf, Just (p, _)) = do
	rff <- showReadFrom rf
	return $ show (ppr pat) ++ ":" ++ rff ++ "[" ++ show (ppr p) ++ "]"
showCheck ((pat, _), rf, Nothing) = do
	rff <- showReadFrom rf
	return $ show (ppr pat) ++ ":" ++ rff

showReadFrom :: ReadFrom -> Q String
showReadFrom (FromVariable (Just v)) = return v
showReadFrom (FromVariable _) = return ""
showReadFrom (FromL l rf) = (++ sl l) <$> showReadFrom rf
	where	sl List = "*"
		sl List1 = "+"
		sl Optional = "?"
showReadFrom (FromSelection sel) = ('(' :) <$> (++ ")") <$> showSelection sel

definitionType :: Peg -> TypeQ -> Definition -> TypeQ
definitionType _ _ (_, Just typ, _) = typ
definitionType peg tk (_, _, sel) = selectionType peg tk sel

selectionType :: Peg -> TypeQ -> Selection -> TypeQ
selectionType peg tk (Right ex) =
	foldr (\x y -> (eitherT `appT` x) `appT` y) (last types) (init types)
	where
	eitherT = conT $ mkName "Either"
	types = map (plainExpressionType peg tk) ex
selectionType _ tk (Left [em]) = do
	e <- em $ mkName "c"
	case e of
		(True, _) ->  tk
		_ -> error "selectionType: can't get type"
selectionType _ _ _ = error "selectionType: can't get type"

plainExpressionType :: Peg -> TypeQ -> PlainExpression -> TypeQ
plainExpressionType peg tk e = let fe = filter ((== Here) . fst) e in
	foldl appT (tupleT $ length fe) $ map (readFromType peg tk . snd) $ fe

readFromType :: Peg -> TypeQ -> ReadFrom -> TypeQ
readFromType peg tk (FromVariable (Just v)) =
	definitionType peg tk $ case filter ((== v) . \(n, _, _) -> n) peg of
		[d] -> d
		_ -> error "searchDefinition: bad"
readFromType peg tk (FromSelection sel) = selectionType peg tk sel
readFromType _ tk (FromVariable _) = tk
readFromType peg tk (FromL l rf) = lt l `appT` readFromType peg tk rf
	where	lt Optional = conT $ mkName "Maybe"
		lt _ = listT

nameFromSelection :: Selection -> Q [String]
nameFromSelection exs = concat <$>
	either (mapM nameFromExpressionQ) (mapM nameFromPlainExpression) exs

nameFromExpressionQ :: ExpressionQ -> Q [String]
nameFromExpressionQ e = e (mkName "c") >>= nameFromExpression

nameFromExpression :: Expression -> Q [String]
nameFromExpression = nameFromCheck . snd . head . fst . snd

nameFromPlainExpression :: PlainExpression -> Q [String]
nameFromPlainExpression = (concat <$>) . mapM (nameFromRF . snd)

nameFromCheck :: Check -> Q [String]
nameFromCheck (_, rf, _) = nameFromRF rf

nameFromRF :: ReadFrom -> Q [String]
nameFromRF (FromVariable (Just s)) = return [s]
nameFromRF (FromVariable _) = return ["char"]
nameFromRF (FromL _ rf) = nameFromRF rf
nameFromRF (FromSelection sel) = nameFromSelection sel

type PegFile = ([PPragma], ModuleName, Maybe Exports, Code, (TypeQ, Peg), Code)
data PPragma = LanguagePragma [String] | OtherPragma String deriving Show
type ModuleName = [String]
type Exports = String
type Code = String

mkPegFile :: [PPragma] -> Maybe ([String], Maybe String) -> String -> String ->
	(TypeQ, Peg) -> String -> PegFile
mkPegFile ps (Just md) x y z w = (ps, fst md, snd md, x ++ "\n" ++ y, z, w)
mkPegFile ps Nothing x y z w = (ps, [], Nothing, x ++ "\n" ++ y, z, w)
