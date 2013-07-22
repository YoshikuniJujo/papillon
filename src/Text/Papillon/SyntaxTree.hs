module Text.Papillon.SyntaxTree where

import Language.Haskell.TH
import Data.Char
import Control.Applicative
import Data.List

data ReadFrom
	= FromVariable String
	| FromSelection Selection
	| FromToken
	| FromList ReadFrom
	| FromList1 ReadFrom
	| FromOptional ReadFrom

getReadFromType :: Peg -> TypeQ -> ReadFrom -> TypeQ
getReadFromType peg tknt (FromVariable var) =
	getDefinitionType peg tknt $ searchDefinition peg var
getReadFromType peg tknt (FromSelection sel) = getSelectionType peg tknt sel
getReadFromType _ tknt FromToken = tknt
getReadFromType peg tknt (FromList rf) = listT `appT` getReadFromType peg tknt rf
getReadFromType peg tknt (FromList1 rf) = listT `appT` getReadFromType peg tknt rf
getReadFromType peg tknt (FromOptional rf) =
	conT (mkName "Maybe") `appT` getReadFromType peg tknt rf

nameFromRF :: ReadFrom -> [String]
nameFromRF (FromVariable s) = [s]
nameFromRF FromToken = ["char"]
nameFromRF (FromList rf) = nameFromRF rf
nameFromRF (FromList1 rf) = nameFromRF rf
nameFromRF (FromOptional rf) = nameFromRF rf
nameFromRF (FromSelection sel) = nameFromSelection sel

showReadFrom :: ReadFrom -> Q String
showReadFrom FromToken = return ""
showReadFrom (FromVariable v) = return v
showReadFrom (FromList rf) = (++ "*") <$> showReadFrom rf
showReadFrom (FromList1 rf) = (++ "+") <$> showReadFrom rf
showReadFrom (FromOptional rf) = (++ "?") <$> showReadFrom rf
showReadFrom (FromSelection sel) = ('(' :) <$> (++ ")") <$> showSelection sel

data NameLeaf = NameLeaf (PatQ, String) ReadFrom (Maybe (ExR, String))

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

data NameLeaf_
	= Here NameLeaf
	| After NameLeaf
	| NotAfter NameLeaf String

showNameLeaf_ :: NameLeaf_ -> Q String
showNameLeaf_ (Here nl) = showNameLeaf nl
showNameLeaf_ (After nl) = ('&' :) <$> showNameLeaf nl
showNameLeaf_ (NotAfter nl _) = ('!' :) <$> showNameLeaf nl

nameFromNameLeaf_ :: NameLeaf_ -> [String]
nameFromNameLeaf_ (Here nl) = nameFromNameLeaf nl
nameFromNameLeaf_ (After nl) = nameFromNameLeaf nl
nameFromNameLeaf_ (NotAfter nl _) = nameFromNameLeaf nl

type Expression = [NameLeaf_]

showExpression :: Expression -> Q String
showExpression ex = unwords <$> mapM showNameLeaf_ ex

nameFromExpression :: Expression -> [String]
nameFromExpression = nameFromNameLeaf_ . head

data ExpressionHs
	= ExpressionHs {
		expressionHsExpression :: Expression,
		expressionHsExR :: ExR
	 }
	| PlainExpressionHs [ReadFrom]

getExpressionHsType :: Peg -> TypeQ -> ExpressionHs -> TypeQ
getExpressionHsType peg tknt (PlainExpressionHs rfs) =
	foldl appT (tupleT $ length rfs) $ map (getReadFromType peg tknt) rfs
getExpressionHsType _ _ _ = error "getExpressionHsType: can't get type"

showExpressionHs :: ExpressionHs -> Q String
showExpressionHs (ExpressionHs ex hs) = do
	expp <- showExpression ex
	hss <- hs
	return $ expp ++ " { " ++ show (ppr hss) ++ " }"
showExpressionHs (PlainExpressionHs rfs) = unwords <$> mapM showReadFrom rfs

nameFromExpressionHs :: ExpressionHs -> [String]
nameFromExpressionHs (ExpressionHs ex _) = nameFromExpression ex
nameFromExpressionHs (PlainExpressionHs rfs) = concatMap nameFromRF rfs

data Selection
	= Selection { expressions :: [ExpressionHs] }
	| PlainSelection { plainExpressions :: [ExpressionHs] }

getSelectionType :: Peg -> TypeQ -> Selection -> TypeQ
getSelectionType peg tknt (PlainSelection ex) =
	foldr (\x y -> (eitherT `appT` x) `appT` y) (conT $ mkName "()") $
		map (getExpressionHsType peg tknt) ex
	where
	eitherT = conT $ mkName "Either"
getSelectionType _ _ _ = error "getSelectionType: can't get type"

showSelection :: Selection -> Q String
showSelection (Selection ehss) = intercalate " / " <$> mapM showExpressionHs ehss
showSelection (PlainSelection ehss) =
	intercalate " / " <$> mapM showExpressionHs ehss

nameFromSelection :: Selection -> [String]
nameFromSelection (Selection exs) = concatMap nameFromExpressionHs exs
nameFromSelection (PlainSelection exs) = concatMap nameFromExpressionHs exs

data Definition
	= Definition String TypeQ Selection
	| PlainDefinition String Selection
type Peg = [Definition]
type TTPeg = (TypeQ, TypeQ, Peg)

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

type Ex = (ExpQ -> ExpQ) -> ExpQ
type ExR = ExpQ
type ExRL = [ExpQ]

type Typ = (TypeQ -> TypeQ) -> TypeQ
type TypeQL = [TypeQ]

tupT :: [TypeQ] -> TypeQ
tupT ts = foldl appT (tupleT $ length ts) ts

getTyp :: Typ -> TypeQ
getTyp t = t id

toTyp :: TypeQ -> Typ
toTyp tp f = f tp

ctLeaf_ :: PatQ -> NameLeaf
ctLeaf_ n = NameLeaf (n, "") FromToken Nothing

true :: ExpQ
true = conE $ mkName "True"

just :: a -> Maybe a
just = Just
nothing :: Maybe a
nothing = Nothing

cons :: a -> [a] -> [a]
cons = (:)

type PatQs = [PatQ]

strToPatQ :: String -> PatQ
strToPatQ = varP . mkName

conToPatQ :: String -> [PatQ] -> PatQ
conToPatQ t = conP (mkName t)

mkExpressionHs :: a -> ExR -> (a, ExR)
mkExpressionHs x y = (x, y)

mkDef :: String -> TypeQ -> Selection -> Definition
mkDef = Definition

isOpTailChar :: Char -> Bool
isOpTailChar = (`elem` ":+*/-!|&.^=<>$")

colon :: Char
colon = ':'

isOpHeadChar :: Char -> Bool
isOpHeadChar = (`elem` "+*/-!|&.^=<>$")

toExp :: String -> Ex
toExp v f = f $ varE (mkName v)

toEx :: ExR -> Ex
toEx v f = f v

apply :: String -> Ex -> Ex
apply f x g = x (toExp f g `appE`)

applyExR :: ExR -> Ex -> Ex
applyExR f x g = x (toEx f g `appE`)

applyTyp :: Typ -> Typ -> Typ
applyTyp f t g = t (f g `appT`)

getEx :: Ex -> ExR
getEx ex = ex id

toExGetEx :: Ex -> Ex
toExGetEx = toEx . getEx

emp :: [a]
emp = []

type PegFile = ([PPragma], ModuleName, Maybe ExportList, Code, TTPeg, Code)
data PPragma = LanguagePragma [String] | OtherPragma String deriving Show
type ModuleName = [String]
type ExportList = String
type Code = String

addModules :: String
addModules =
	"import \"monads-tf\" Control.Monad.State\n" ++
	"import \"monads-tf\" Control.Monad.Error\n"

mkPegFile :: [PPragma] -> Maybe ([String], Maybe String) -> String -> String ->
	TTPeg -> String -> PegFile
mkPegFile ps (Just md) x y z w = (
	ps,
	fst md,
	snd md,
	addModules ++
	x ++ "\n" ++ y, z, w)
mkPegFile ps Nothing x y z w =
	(ps, [], Nothing, addModules ++ x ++ "\n" ++ y, z, w)

charP :: Char -> PatQ
charP = litP . charL
stringP :: String -> PatQ
stringP = litP . stringL

isStrLitC, isAlphaNumOt, elemNTs :: Char -> Bool
isAlphaNumOt = (`notElem` "\\'")
elemNTs = (`elem` "nt\\'")
isStrLitC = (`notElem` "\"\\")

tab :: Char
tab = '\t'

isComma, isKome, isOpen, isClose, isGt, isQuestion, isBQ, isAmp :: Char -> Bool
isComma = (== ',')
isKome = (== '*')
isOpen = (== '(')
isClose = (== ')')
isGt = (== '>')
isQuestion = (== '?')
isBQ = (== '`')
isAmp = (== '&')

getNTs :: Char -> Char
getNTs 'n' = '\n'
getNTs 't' = '\t'
getNTs '\\' = '\\'
getNTs '\'' = '\''
getNTs o = o
isLowerU :: Char -> Bool
isLowerU c = isLower c || c == '_'

tString :: String
tString = "String"
mkTTPeg :: String -> Peg -> TTPeg
mkTTPeg s p =
	(conT $ mkName s, conT (mkName "Token") `appT` conT (mkName s), p)
