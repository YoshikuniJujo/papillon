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

nameFromRF :: ReadFrom -> [String]
nameFromRF (FromVariable s) = [s]
nameFromRF FromToken = ["derivsChars"]
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

type ExpressionHs = (Expression, ExR)

showExpressionHs :: ExpressionHs -> Q String
showExpressionHs (ex, hs) = do
	expp <- showExpression ex
	hss <- hs
	return $ expp ++ " { " ++ show (ppr hss) ++ " }"

nameFromExpressionHs :: ExpressionHs -> [String]
nameFromExpressionHs = nameFromExpression . fst

type Selection = [ExpressionHs]

showSelection :: Selection -> Q String
showSelection ehss = intercalate " / " <$> mapM showExpressionHs ehss

nameFromSelection :: Selection -> [String]
nameFromSelection = concatMap nameFromExpressionHs

type Definition = (String, TypeQ, Selection)
type Peg = [Definition]
type TTPeg = (TypeQ, TypeQ, Peg)

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

mkDef :: a -> TypeQ -> c -> (a, TypeQ, c)
mkDef x y z = (x, y, z)

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

type PegFile = ([PPragma], ModuleName, String, String, TTPeg, String)
data PPragma = LanguagePragma [String] | OtherPragma String deriving Show
type ModuleName = [String]

addModules :: String
addModules =
	"import \"monads-tf\" Control.Monad.State\n" ++
	"import \"monads-tf\" Control.Monad.Error\n"

correctMD :: ([String], String) -> String
correctMD (n, o) = intercalate "." n ++ o
mkPegFile :: [PPragma] -> Maybe ([String], String) -> String -> String ->
	TTPeg -> String -> PegFile
mkPegFile ps (Just md) x y z w = (
	ps,
	fst md,
	snd md ++ " where\n" ++
	addModules,
	x ++ "\n" ++ y, z, w)
mkPegFile ps Nothing x y z w =
	(ps, [], addModules, x ++ "\n" ++ y, z, w)

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
