{-# LANGUAGE TupleSections #-}

module Text.Papillon.SyntaxTree (
	HA(..),
	Lists(..),

	Peg,
	Definition,
	Selection,
	PlainExpression,
	Expression,
	NameLeaf,
	ReadFrom(..),
	fromTokenChars,
	expressionSugar,

	showSelection,
	showNameLeaf,
	getDefinitionType,
	getSelectionType,
	nameFromSelection,
	nameFromRF,

	PegFile,
	mkPegFile,
	PPragma(..),
	ModuleName,
	ExportList,
	Code
) where

import Language.Haskell.TH
import Control.Applicative
import Control.Arrow
import Data.List
import Data.Maybe

data HA = Here | After | NotAfter String deriving (Show, Eq)
data Lists = List | List1 | Optional deriving Show

type Peg = [Definition]
type Definition = (String, Maybe TypeQ, Selection)
type Selection =  Either [Expression] [PlainExpression]
type Expression = (Bool, (Name -> ([(HA, NameLeaf)], ExpQ)))
type PlainExpression = [(HA, ReadFrom)]
type NameLeaf = ((PatQ, String), ReadFrom, Maybe (ExpQ, String))
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
		<$> mapM (uncurry (<$>) . (((++) . showHA) *** showNameLeaf)) ex
		<*> hs

showPlainExpression :: PlainExpression -> Q String
showPlainExpression rfs =
	unwords <$> mapM (\(ha, rf) -> (showHA ha ++) <$> showReadFrom rf) rfs

showHA :: HA -> String
showHA Here = ""
showHA After = "&"
showHA (NotAfter _) = "!"

showNameLeaf :: NameLeaf -> Q String
showNameLeaf ((pat, _), rf, Just (p, _)) = do
	patt <- pat
	rff <- showReadFrom rf
	pp <- p
	return $ show (ppr patt) ++ ":" ++ rff ++ "[" ++ show (ppr pp) ++ "]"
showNameLeaf ((pat, _), rf, Nothing) = do
	patt <- pat
	rff <- showReadFrom rf
	return $ show (ppr patt) ++ ":" ++ rff

showLists :: Lists -> String
showLists List = "*"
showLists List1 = "+"
showLists Optional = "?"

showReadFrom :: ReadFrom -> Q String
showReadFrom (FromVariable (Just v)) = return v
showReadFrom (FromVariable _) = return ""
showReadFrom (FromL l rf) = (++ showLists l) <$> showReadFrom rf
showReadFrom (FromSelection sel) = ('(' :) <$> (++ ")") <$> showSelection sel

getDefinitionType :: Peg -> TypeQ -> Definition -> TypeQ
getDefinitionType _ _ (_, (Just typ), _) = typ
getDefinitionType peg tknt (_, _, sel) = getSelectionType peg tknt sel

getSelectionType :: Peg -> TypeQ -> Selection -> TypeQ
getSelectionType peg tknt (Right ex) =
	foldr (\x y -> (eitherT `appT` x) `appT` y) (last types) (init types)
	where
	eitherT = conT $ mkName "Either"
	types = map (getPlainExpressionType peg tknt) ex
getSelectionType _ tknt (Left [(True, _)]) = tknt
getSelectionType _ _ _ = error "getSelectionType: can't get type"

getPlainExpressionType :: Peg -> TypeQ -> PlainExpression -> TypeQ
getPlainExpressionType peg tknt rfs =
	foldl appT (tupleT $ length $ filter ((== Here) . fst) rfs) $ catMaybes $
		map (getHAReadFromType peg tknt) rfs

getHAReadFromType :: Peg -> TypeQ -> (HA, ReadFrom) -> Maybe TypeQ
getHAReadFromType peg tknt (Here, rf) = Just $ getReadFromType peg tknt rf
getHAReadFromType _ _ _ = Nothing

getListsType :: Lists -> TypeQ
getListsType Optional = conT $ mkName "Maybe"
getListsType _ = listT

getReadFromType :: Peg -> TypeQ -> ReadFrom -> TypeQ
getReadFromType peg tknt (FromVariable (Just var)) =
	getDefinitionType peg tknt $ searchDefinition peg var
getReadFromType peg tknt (FromSelection sel) = getSelectionType peg tknt sel
getReadFromType _ tknt (FromVariable _) = tknt
getReadFromType peg tknt (FromL l rf) =
	getListsType l `appT` getReadFromType peg tknt rf

searchDefinition :: Peg -> String -> Definition
searchDefinition peg var = case filter ((== var) . \(n, _, _) -> n) peg of
	[d] -> d
	_ -> error "searchDefinition: bad"

nameFromSelection :: Selection -> [String]
nameFromSelection (Left exs) = concatMap nameFromExpression exs
nameFromSelection (Right exs) = concatMap nameFromPlainExpression exs

nameFromExpression :: Expression -> [String]
nameFromExpression (_, exhs) = let (ex, _) = exhs $ mkName "c" in
	nameFromNameLeaf $ snd $ head ex

nameFromPlainExpression :: PlainExpression -> [String]
nameFromPlainExpression rfs = concatMap (nameFromRF . snd) rfs

nameFromNameLeaf :: NameLeaf -> [String]
nameFromNameLeaf (_, rf, _) = nameFromRF rf

nameFromRF :: ReadFrom -> [String]
nameFromRF (FromVariable (Just s)) = [s]
nameFromRF (FromVariable _) = ["char"]
nameFromRF (FromL _ rf) = nameFromRF rf
nameFromRF (FromSelection sel) = nameFromSelection sel

type PegFile =
	([PPragma], ModuleName, Maybe ExportList, Code, (TypeQ, Peg), Code)
data PPragma = LanguagePragma [String] | OtherPragma String deriving Show
type ModuleName = [String]
type ExportList = String
type Code = String

mkPegFile :: [PPragma] -> Maybe ([String], Maybe String) -> String -> String ->
	(TypeQ, Peg) -> String -> PegFile
mkPegFile ps (Just md) x y z w = (ps, fst md, snd md, x ++ "\n" ++ y, z, w)
mkPegFile ps Nothing x y z w = (ps, [], Nothing, x ++ "\n" ++ y, z, w)
