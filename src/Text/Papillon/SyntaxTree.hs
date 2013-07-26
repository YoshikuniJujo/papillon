{-# LANGUAGE TupleSections #-}

module Text.Papillon.SyntaxTree (
	Peg,
	Definition,
	Selection,
	Expression,
	PlainExpression,
	Check,
	ReadFrom(..),

	DefinitionQ,
	SelectionQ,
	ExpressionQ,
	CheckQ,

	definitionQ,
	normalSelectionQ,
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
import Control.Monad
import Control.Applicative
import Control.Arrow
import Data.List
-- import Data.Maybe
import Data.IORef

data Lookahead = Here | Ahead | NAhead String deriving Eq
data Lists = List | List1 | Optional deriving Show

type Peg = [DefinitionQ]
type Definition = (String, Maybe Type, Selection)

definitionQ :: String -> Maybe TypeQ -> SelectionQ -> DefinitionQ
definitionQ name typq selq = \g -> do
	sel <- selq g
	typ <- case typq of
		Just t -> Just <$> t
		_ -> return Nothing
	return $ (name, typ, sel)

type Selection =  Either [Expression] [PlainExpression]
type Expression = (Bool, ([(Lookahead, Check)], Exp))
type PlainExpression = [(Lookahead, ReadFrom)]
type Check = ((Pat, String), ReadFrom, Maybe (Exp, String))
data ReadFrom
	= FromVariable (Maybe String)
	| FromSelection SelectionQ
	| FromL Lists ReadFrom

type DefinitionQ = IORef Int -> Q Definition
type SelectionQ = IORef Int -> Q Selection
type ExpressionQ = Name -> Q Expression
type CheckQ = Q Check

normalSelectionQ :: [ExpressionQ] -> SelectionQ
normalSelectionQ expqs = \g -> (Left <$>) $ forM expqs $ \e -> do
	c <- newNewName g "c"
	e c

newNewName :: IORef Int -> String -> Q Name
newNewName g n = do
	s <- runIO $ readIORef g
	newName $ n ++ show s

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
fromTokenChars cs = FromSelection $ \g -> do
	s <- runIO $ readIORef g
	c <- newName $ "c" ++ show s
	ex <- (expressionSugar $
		infixE Nothing (varE $ mkName "elem") $ Just $ litE $ stringL cs) c
	return $ Left [ex]
showSelection :: SelectionQ -> Q String
showSelection ehss = do
	g <- runIO $ newIORef 0
	ehs <- ehss g
	intercalate " / " <$>
		either (mapM showExpression) (mapM showPlainExpression) ehs

{-
showExpressionQ :: ExpressionQ -> Q String
showExpressionQ e = e (mkName "c") >>= showExpression
-}

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

definitionType :: Peg -> TypeQ -> DefinitionQ -> TypeQ
definitionType peg tk defq = do
	def <- defq =<< runIO (newIORef 0)
	case def of
		(_, Just typ, _) -> return typ
		(_, _, sel) -> selectionType peg tk sel

selectionTypeQ :: Peg -> TypeQ -> SelectionQ -> TypeQ
selectionTypeQ peg tk sel = do
	e <- sel =<< runIO (newIORef 0)
	selectionType peg tk e

selectionType :: Peg -> TypeQ -> Selection -> TypeQ
{-
selectionType peg tk sel = do
	Right ex <- sel $ mkName "c"
	foldr (\x y -> (eitherT `appT` x) `appT` y)
		(last $ types ex) (init $ types ex)
	where
	eitherT = conT $ mkName "Either"
	types e = map (plainExpressionType peg tk) e
-}
selectionType peg tk e = do
--	e <- sel =<< runIO (newIORef 0)
	case e of
		Right ex -> foldr (\x y -> (eitherT `appT` x) `appT` y)
			(last $ types ex) (init $ types ex)
		Left [(True, _)] ->  tk
		_ -> error "selectionType: can't get type"
	where
	eitherT = conT $ mkName "Either"
	types e' = map (plainExpressionType peg tk) e'
-- selectionType _ _ _ = error "selectionType: can't get type"

plainExpressionType :: Peg -> TypeQ -> PlainExpression -> TypeQ
plainExpressionType peg tk e = let fe = filter ((== Here) . fst) e in
	foldl appT (tupleT $ length fe) $ map (readFromType peg tk . snd) $ fe

readFromType :: Peg -> TypeQ -> ReadFrom -> TypeQ
readFromType peg tk (FromVariable (Just v)) =
	definitionType peg tk $ searchDefinition peg v
readFromType peg tk (FromSelection sel) = selectionTypeQ peg tk sel
readFromType _ tk (FromVariable _) = tk
readFromType peg tk (FromL l rf) = lt l `appT` readFromType peg tk rf
	where	lt Optional = conT $ mkName "Maybe"
		lt _ = listT

searchDefinition :: Peg -> String -> DefinitionQ
searchDefinition peg name = \g -> do
	ds <- flip filterM peg $
		((== name) . (\(n, _, _) -> n) <$>) . ($ g)
	case ds of
		[d] -> d g
		_ -> error "searchDefinition: bad"

nameFromSelection :: SelectionQ -> Q [String]
nameFromSelection exs = concat <$>
	(either (mapM nameFromExpression) (mapM nameFromPlainExpression)
		=<< exs =<< runIO (newIORef 0))

{-
nameFromExpressionQ :: ExpressionQ -> Q [String]
nameFromExpressionQ e = e (mkName "c") >>= nameFromExpression
-}

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
