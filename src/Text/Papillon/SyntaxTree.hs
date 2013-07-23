module Text.Papillon.SyntaxTree (
	Peg,
	Definition(..),
	Selection(..),
	Expression(..),
	NameLeaf_(..),
	NameLeaf(..),
	ReadFrom(..),

	getDefinitionType,
	getSelectionType,
	showSelection,
	showNameLeaf,
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
import Data.List

type Peg = [Definition]
data Definition
	= Definition String TypeQ Selection
	| PlainDefinition String Selection
data Selection
	= Selection { expressions :: [Expression] }
	| PlainSelection { plainExpressions :: [Expression] }
data Expression
	= Expression {
		expressionHsExpression :: [NameLeaf_],
		expressionHsExR :: ExpQ
	 }
	| ExpressionSugar ExpQ
	| PlainExpression [ReadFrom]
data NameLeaf_
	= Here NameLeaf
	| After NameLeaf
	| NotAfter NameLeaf String
data NameLeaf = NameLeaf (PatQ, String) ReadFrom (Maybe (ExpQ, String))
data ReadFrom
	= FromVariable String
	| FromSelection Selection
	| FromToken
	| FromTokenChars [Char]
	| FromList ReadFrom
	| FromList1 ReadFrom
	| FromOptional ReadFrom

getReadFromType :: Peg -> TypeQ -> ReadFrom -> TypeQ
getReadFromType peg tknt (FromVariable var) =
	getDefinitionType peg tknt $ searchDefinition peg var
getReadFromType peg tknt (FromSelection sel) = getSelectionType peg tknt sel
getReadFromType _ tknt FromToken = tknt
getReadFromType _ tknt (FromTokenChars _) = tknt
getReadFromType peg tknt (FromList rf) = listT `appT` getReadFromType peg tknt rf
getReadFromType peg tknt (FromList1 rf) = listT `appT` getReadFromType peg tknt rf
getReadFromType peg tknt (FromOptional rf) =
	conT (mkName "Maybe") `appT` getReadFromType peg tknt rf

nameFromRF :: ReadFrom -> [String]
nameFromRF (FromVariable s) = [s]
nameFromRF FromToken = ["char"]
nameFromRF (FromTokenChars _) = ["char"]
nameFromRF (FromList rf) = nameFromRF rf
nameFromRF (FromList1 rf) = nameFromRF rf
nameFromRF (FromOptional rf) = nameFromRF rf
nameFromRF (FromSelection sel) = nameFromSelection sel

showReadFrom :: ReadFrom -> Q String
showReadFrom FromToken = return ""
showReadFrom (FromTokenChars cs) = return $ '[' : cs ++ "]"
showReadFrom (FromVariable v) = return v
showReadFrom (FromList rf) = (++ "*") <$> showReadFrom rf
showReadFrom (FromList1 rf) = (++ "+") <$> showReadFrom rf
showReadFrom (FromOptional rf) = (++ "?") <$> showReadFrom rf
showReadFrom (FromSelection sel) = ('(' :) <$> (++ ")") <$> showSelection sel

showNameLeaf :: NameLeaf -> Q String
showNameLeaf (NameLeaf (pat, _) rf (Just (p, _))) = do
	patt <- pat
	rff <- showReadFrom rf
	pp <- p
	return $ show (ppr patt) ++ ":" ++ rff ++ "[" ++ show (ppr pp) ++ "]"
showNameLeaf (NameLeaf (pat, _) rf Nothing) = do
	patt <- pat
	rff <- showReadFrom rf
	return $ show (ppr patt) ++ ":" ++ rff

nameFromNameLeaf :: NameLeaf -> [String]
nameFromNameLeaf (NameLeaf _ rf _) = nameFromRF rf

showNameLeaf_ :: NameLeaf_ -> Q String
showNameLeaf_ (Here nl) = showNameLeaf nl
showNameLeaf_ (After nl) = ('&' :) <$> showNameLeaf nl
showNameLeaf_ (NotAfter nl _) = ('!' :) <$> showNameLeaf nl

nameFromNameLeaf_ :: NameLeaf_ -> [String]
nameFromNameLeaf_ (Here nl) = nameFromNameLeaf nl
nameFromNameLeaf_ (After nl) = nameFromNameLeaf nl
nameFromNameLeaf_ (NotAfter nl _) = nameFromNameLeaf nl

showExpression :: [NameLeaf_] -> Q String
showExpression ex = unwords <$> mapM showNameLeaf_ ex

nameFromExpression :: [NameLeaf_] -> [String]
nameFromExpression = nameFromNameLeaf_ . head

getExpressionType :: Peg -> TypeQ -> Expression -> TypeQ
getExpressionType peg tknt (PlainExpression rfs) =
	foldl appT (tupleT $ length rfs) $ map (getReadFromType peg tknt) rfs
getExpressionType _ _ _ = error "getExpressionType: can't get type"

showExpressionHs :: Expression -> Q String
showExpressionHs (Expression ex hs) = do
	expp <- showExpression ex
	hss <- hs
	return $ expp ++ " { " ++ show (ppr hss) ++ " }"
showExpressionHs (ExpressionSugar hs) = do
	hss <- hs
	return $ "<" ++ show (ppr hss) ++ ">"
showExpressionHs (PlainExpression rfs) = unwords <$> mapM showReadFrom rfs

nameFromExpressionHs :: Expression -> [String]
nameFromExpressionHs (Expression ex _) = nameFromExpression ex
nameFromExpressionHs (ExpressionSugar _) = []
nameFromExpressionHs (PlainExpression rfs) = concatMap nameFromRF rfs

getSelectionType :: Peg -> TypeQ -> Selection -> TypeQ
getSelectionType peg tknt (PlainSelection ex) =
	foldr (\x y -> (eitherT `appT` x) `appT` y) (last types) (init types)
	where
	eitherT = conT $ mkName "Either"
	types = map (getExpressionType peg tknt) ex
getSelectionType _ _ _ = error "getSelectionType: can't get type"

showSelection :: Selection -> Q String
showSelection (Selection ehss) = intercalate " / " <$> mapM showExpressionHs ehss
showSelection (PlainSelection ehss) =
	intercalate " / " <$> mapM showExpressionHs ehss

nameFromSelection :: Selection -> [String]
nameFromSelection (Selection exs) = concatMap nameFromExpressionHs exs
nameFromSelection (PlainSelection exs) = concatMap nameFromExpressionHs exs

searchDefinition :: Peg -> String -> Definition
searchDefinition peg var = case filter ((== var) . getDefinitionName) peg of
	[d] -> d
	_ -> error "searchDefinition: bad"

getDefinitionName :: Definition -> String
getDefinitionName (Definition n _ _) = n
getDefinitionName (PlainDefinition n _) = n

getDefinitionType :: Peg -> TypeQ -> Definition -> TypeQ
getDefinitionType _ _ (Definition _ typ _) = typ
getDefinitionType peg tknt (PlainDefinition _ sel) = getSelectionType peg tknt sel

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
