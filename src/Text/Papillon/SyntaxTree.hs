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
	selectionTypeQ,
	showCheck,
	showCheckQ,
	nameFromRF,
	nameFromRFQ,

	PegFile,
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
-- import Data.Maybe
import Data.IORef

data Lookahead = Here | Ahead | NAhead String deriving (Show, Eq)
data Lists = List | List1 | Optional deriving Show

type STPeg = (Type, Peg)
type Peg = [Definition]
type Definition = (String, Maybe Type, Selection)
type Selection =  Either [Expression] [PlainExpression]
type Expression = (Bool, ([(Lookahead, Check)], Exp))
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

{-
newNewName :: IORef Int -> String -> Q Name
newNewName g n = do
	s <- runIO $ readIORef g
	newName $ n ++ show s
-}

expressionQ :: Bool -> ([(Lookahead, CheckQ)], ExpQ) -> ExpressionQ
expressionQ b (ls, ex) g = do
	e <- ex
	l <- mapM (\(la, c) -> (la ,) <$> c g) ls
	return (b, (l, e))

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
	return $ (True ,) $ (, VarE c) $ (: []) $ (Here ,) $ (,,)
		(VarP c, "") (FromVariable Nothing) (Just $ (, "") $ p `AppE` VarE c)

fromTokenChars :: String -> ReadFromQ
fromTokenChars cs = \g -> do
--	s <- runIO $ readIORef g
--	c <- newName $ "c" ++ show s
	ex <- flip expressionSugar g $ infixE Nothing (varE $ mkName "elem") $
		Just $ litE $ stringL cs
	return $ FromSelection $ Left [ex]

showSelection :: Selection -> Q String
showSelection ehs = intercalate " / " <$>
	either (mapM showExpression) (mapM showPlainExpression) ehs

showExpression :: Expression -> Q String
showExpression (_, exhs) = let (ex, hs) = exhs in
	(\e -> unwords e ++ " { " ++ show (ppr hs) ++ " }")
		<$> mapM (uncurry (<$>) . (((++) . showLA) *** showCheck)) ex

showPlainExpression :: PlainExpression -> Q String
showPlainExpression rfs =
	unwords <$> mapM (\(ha, rf) -> (showLA ha ++) <$> showReadFrom rf) rfs

showLA :: Lookahead -> String
showLA Here = ""
showLA Ahead = "&"
showLA (NAhead _) = "!"

showCheckQ :: CheckQ -> Q String
showCheckQ ckq = showCheck =<< ckq =<< runIO (newIORef 0)

showCheck :: Check -> Q String
showCheck ((pat, _), rf, Just (p, _)) = do
	rff <- showReadFrom rf
	return $ show (ppr pat) ++ ":" ++ rff ++ "[" ++ show (ppr p) ++ "]"
showCheck ((pat, _), rf, Nothing) = do
	rff <- showReadFrom rf
	return $ show (ppr pat) ++ ":" ++ rff

{-
showReadFromQ :: ReadFromQ -> Q String
showReadFromQ rfq = showReadFrom =<< rfq =<< runIO (newIORef 0)
-}

showReadFrom :: ReadFrom -> Q String
showReadFrom (FromVariable (Just v)) = return v
showReadFrom (FromVariable _) = return ""
showReadFrom (FromL l rf) = (++ sl l) <$> showReadFrom rf
	where	sl List = "*"
		sl List1 = "+"
		sl Optional = "?"
showReadFrom (FromSelection sel) = ('(' :) <$> (++ ")") <$> showSelection sel

definitionType :: PegQ -> TypeQ -> DefinitionQ -> TypeQ
definitionType pegq tk defq = do
	def <- defq =<< runIO (newIORef 0)
	case def of
		(_, Just typ, _) -> return typ
		(_, _, sel) -> selectionType pegq tk sel

selectionTypeQ :: PegQ -> TypeQ -> SelectionQ -> TypeQ
selectionTypeQ peg tk sel = do
	e <- sel =<< runIO (newIORef 0)
	selectionType peg tk e

selectionType :: PegQ -> TypeQ -> Selection -> TypeQ
selectionType peg tk e = do
	case e of
		Right ex -> foldr (\x y -> (eitherT `appT` x) `appT` y)
			(last $ types ex) (init $ types ex)
		Left [(True, _)] ->  tk
		_ -> error "selectionType: can't get type"
	where
	eitherT = conT $ mkName "Either"
	types e' = map (plainExpressionType peg tk) e'

plainExpressionType :: PegQ -> TypeQ -> PlainExpression -> TypeQ
plainExpressionType peg tk e = let fe = filter ((== Here) . fst) e in
	foldl appT (tupleT $ length fe) $ map (readFromType peg tk . snd) $ fe

{-
readFromTypeQ :: PegQ -> TypeQ -> ReadFromQ -> TypeQ
readFromTypeQ peg tk rfq = readFromType peg tk =<< rfq =<< runIO (newIORef 0)
-}

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

nameFromRFQ :: ReadFromQ -> Q [String]
nameFromRFQ rfq = nameFromRF =<< rfq =<< runIO (newIORef 0)

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
