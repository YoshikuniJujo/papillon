{-# LANGUAGE TupleSections #-}

module Text.Papillon.SyntaxTree (
	Peg,
	Definition,
	Selection,
	Expression,
	PlainExpression,
	Check,
	ReadFrom(..),

	Lookahead(..),
	Lists(..),

	fromTokenChars,
	expressionSugar,

	selectionType,
	showCheck,
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
type Selection =  Either [Expression] [PlainExpression]
type Expression = (Bool, (Name -> ([(Lookahead, Check)], ExpQ)))
type PlainExpression = [(Lookahead, ReadFrom)]
type Check = ((PatQ, String), ReadFrom, Maybe (ExpQ, String))
data ReadFrom
	= FromVariable (Maybe String)
	| FromSelection Selection
	| FromL Lists ReadFrom

expressionSugar :: ExpQ -> Expression
expressionSugar p = (True ,) $ \c -> (, varE c) $ (: []) $ (Here ,) $
	((varP c, ""), (FromVariable Nothing), Just $ (, "") $ p `appE` varE c)

fromTokenChars :: String -> ReadFrom
fromTokenChars cs = FromSelection $ Left $ (: []) $ expressionSugar $
	infixE Nothing (varE $ mkName "elem") $ Just $ litE $ stringL cs

showSelection :: Selection -> Q String
showSelection ehss = intercalate " / " <$>
	either (mapM showExpression) (mapM showPlainExpression) ehss

showExpression :: Expression -> Q String
showExpression (_, exhs) = let (ex, hs) = exhs $ mkName "c" in
	(\e h -> unwords e ++ " { " ++ show (ppr h) ++ " }")
		<$> mapM (uncurry (<$>) . (((++) . showLA) *** showCheck)) ex
		<*> hs

showPlainExpression :: PlainExpression -> Q String
showPlainExpression rfs =
	unwords <$> mapM (\(ha, rf) -> (showLA ha ++) <$> showReadFrom rf) rfs

showLA :: Lookahead -> String
showLA Here = ""
showLA Ahead = "&"
showLA (NAhead _) = "!"

showCheck :: Check -> Q String
showCheck ((pat, _), rf, Just (p, _)) = do
	patt <- pat
	rff <- showReadFrom rf
	pp <- p
	return $ show (ppr patt) ++ ":" ++ rff ++ "[" ++ show (ppr pp) ++ "]"
showCheck ((pat, _), rf, Nothing) = do
	patt <- pat
	rff <- showReadFrom rf
	return $ show (ppr patt) ++ ":" ++ rff

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
selectionType _ tk (Left [(True, _)]) = tk
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

nameFromSelection :: Selection -> [String]
nameFromSelection exs = concat $
	either (map nameFromExpression) (map nameFromPlainExpression) exs

nameFromExpression :: Expression -> [String]
nameFromExpression = nameFromCheck . snd . head . fst . ($ mkName "c") . snd

nameFromPlainExpression :: PlainExpression -> [String]
nameFromPlainExpression = concatMap (nameFromRF . snd)

nameFromCheck :: Check -> [String]
nameFromCheck (_, rf, _) = nameFromRF rf

nameFromRF :: ReadFrom -> [String]
nameFromRF (FromVariable (Just s)) = [s]
nameFromRF (FromVariable _) = ["char"]
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
