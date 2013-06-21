{-# LANGUAGE FlexibleContexts, TemplateHaskell , FlexibleContexts, PackageImports, TypeFamilies, RankNTypes #-}
module  Text.Papillon.Parser (
	Peg,
	Definition,
	Selection,
	ExpressionHs,
	NameLeaf(..),
	NameLeaf_(..),
	parse,
	dv_peg,
	dv_pegFile,
)  where
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Monad.Trans.Error (Error (..))
import Control.Applicative


import Data.Char
import Language.Haskell.TH

type MaybeString = Maybe String

type Nil = ()
-- type Leaf = Either String ExR
type Leaf = (Maybe String, ExR)
data NameLeaf
	= NameLeaf PatQ Leaf
	| NameLeafList PatQ Selection
data NameLeaf_ = NotAfter NameLeaf | Here NameLeaf
notAfter, here :: NameLeaf -> NameLeaf_
notAfter = NotAfter
here = Here
type Expression = [NameLeaf_]
type ExpressionHs = (Expression, ExR)
type Selection = [ExpressionHs]
type Typ = Name
type Definition = (String, Typ, Selection)
type Peg = [Definition]
type TTPeg = (TypeQ, TypeQ, Peg)

type Ex = ExpQ -> ExpQ
type ExR = ExpQ

ctLeaf :: Leaf
-- ctLeaf = Right $ conE (mkName "True") -- varE (mkName "const") `appE` conE (mkName "True")
ctLeaf = (Nothing, conE $ mkName "True")

ruleLeaf :: String -> ExpQ -> Leaf
boolLeaf :: ExpQ -> Leaf
ruleLeaf r t = (Just r, t) -- Left
boolLeaf p = (Nothing, p) -- Right

true :: ExpQ
true = conE $ mkName "True"

just :: a -> Maybe a
just = Just
nothing :: Maybe a
nothing = Nothing

nil :: Nil
nil = ()

cons :: a -> [a] -> [a]
cons = (:)

type PatQs = [PatQ]

mkNameLeaf :: PatQ -> Leaf -> NameLeaf
mkNameLeaf = NameLeaf

mkNameLeafList :: PatQ -> Selection -> NameLeaf
mkNameLeafList = NameLeafList

strToPatQ :: String -> PatQ
strToPatQ = varP . mkName

conToPatQ :: String -> [PatQ] -> PatQ
conToPatQ t ps = conP (mkName t) ps

mkExpressionHs :: a -> Ex -> (a, ExR)
mkExpressionHs x y = (x, getEx y)

mkDef :: a -> String -> c -> (a, Name, c)
mkDef x y z = (x, mkName y, z)

toExp :: String -> Ex
toExp v = \f -> f `appE` varE (mkName v)

apply :: String -> Ex -> Ex
apply f x = \g -> x (toExp f g)

getEx :: Ex -> ExR
getEx ex = ex (varE $ mkName "id")

emp :: [a]
emp = []

type PegFile = (String, TTPeg, String)
mkPegFile :: Maybe String -> Maybe String -> String -> String -> b -> c -> (String, b, c)
mkPegFile (Just p) (Just md) x y z w =
	("{-#" ++ p ++ addPragmas ++ "module " ++ md ++ " where\n" ++
	addModules ++
	x ++ "\n" ++ y, z, w)
mkPegFile Nothing (Just md) x y z w =
	(x ++ "\n" ++ "module " ++ md ++ " where\n" ++
	addModules ++
	x ++ "\n" ++ y, z, w)
mkPegFile (Just p) Nothing x y z w = (
	"{-#" ++ p ++ addPragmas ++
	addModules ++
	x ++ "\n" ++ y
	, z, w)
mkPegFile Nothing Nothing x y z w = (addModules ++ x ++ "\n" ++ y, z, w)

addPragmas, addModules :: String
addPragmas =
	", FlexibleContexts, PackageImports, TypeFamilies, RankNTypes #-}\n"
addModules =
	"import \"monads-tf\" Control.Monad.State\n" ++
	"import \"monads-tf\" Control.Monad.Error\n" ++
	"import Control.Monad.Trans.Error (Error (..))\n" ++
	"import Control.Applicative"

charP :: Char -> PatQ
charP = litP . charL
stringP :: String -> PatQ
stringP = litP . stringL

isAlphaNumOt, elemNTs :: Char -> Bool
isAlphaNumOt c = isAlphaNum c || c `elem` "{-#.\":}|[]!;=/ *()"
elemNTs = (`elem` "nt\\'")

isKome, isOpen, isClose :: Char -> Bool
isKome = (== '*')
isOpen = (== '(')
isClose = (== ')')

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

flipMaybe :: (Error (ErrorType me), MonadError me) =>
	StateT s me a -> StateT s me ()
flipMaybe action = do
	err <- (action >> return False) `catchError` const (return True)
	unless err $ throwError $ strMsg "not error"
type PackratM = StateT Derivs (Either String)
type Result v = Either String ((v, Derivs))
data Derivs
    = Derivs {dv_pegFile :: (Result PegFile),
              dv_pragma :: (Result MaybeString),
              dv_pragmaStr :: (Result String),
              dv_pragmaEnd :: (Result Nil),
              dv_moduleDec :: (Result MaybeString),
              dv_moduleDecStr :: (Result String),
              dv_whr :: (Result Nil),
              dv_preImpPap :: (Result String),
              dv_prePeg :: (Result String),
              dv_afterPeg :: (Result String),
              dv_importPapillon :: (Result Nil),
              dv_varToken :: (Result String),
              dv_typToken :: (Result String),
              dv_pap :: (Result Nil),
              dv_peg :: (Result TTPeg),
              dv_sourceType :: (Result String),
              dv_peg_ :: (Result Peg),
              dv_definition :: (Result Definition),
              dv_selection :: (Result Selection),
              dv_expressionHs :: (Result ExpressionHs),
              dv_expression :: (Result Expression),
              dv_nameLeaf_ :: (Result NameLeaf_),
              dv_nameLeaf :: (Result NameLeaf),
              dv_pat :: (Result PatQ),
              dv_charLit :: (Result Char),
              dv_stringLit :: (Result String),
              dv_dq :: (Result Nil),
              dv_pats :: (Result PatQs),
              dv_leaf :: (Result Leaf),
              dv_test :: (Result ExR),
              dv_hsExp :: (Result Ex),
              dv_typ :: (Result String),
              dv_variable :: (Result String),
              dv_tvtail :: (Result String),
              dv_alpha :: (Result Char),
              dv_upper :: (Result Char),
              dv_lower :: (Result Char),
              dv_digit :: (Result Char),
              dv_spaces :: (Result Nil),
              dv_space :: (Result Nil),
              dv_notNLString :: (Result String),
              dv_nl :: (Result Nil),
              dv_comment :: (Result Nil),
              dv_comments :: (Result Nil),
              dv_notComStr :: (Result Nil),
              dv_comEnd :: (Result Nil),
              dvChars :: (Result (Token String))}
parse :: String -> Derivs
parse s = d
          where d = Derivs pegFile pragma pragmaStr pragmaEnd moduleDec moduleDecStr whr preImpPap prePeg afterPeg importPapillon varToken typToken pap peg sourceType peg_ definition selection expressionHs expression nameLeaf_ nameLeaf pat charLit stringLit dq pats leaf test hsExp typ variable tvtail alpha upper lower digit spaces space notNLString nl comment comments notComStr comEnd char
                pegFile = runStateT p_pegFile d
                pragma = runStateT p_pragma d
                pragmaStr = runStateT p_pragmaStr d
                pragmaEnd = runStateT p_pragmaEnd d
                moduleDec = runStateT p_moduleDec d
                moduleDecStr = runStateT p_moduleDecStr d
                whr = runStateT p_whr d
                preImpPap = runStateT p_preImpPap d
                prePeg = runStateT p_prePeg d
                afterPeg = runStateT p_afterPeg d
                importPapillon = runStateT p_importPapillon d
                varToken = runStateT p_varToken d
                typToken = runStateT p_typToken d
                pap = runStateT p_pap d
                peg = runStateT p_peg d
                sourceType = runStateT p_sourceType d
                peg_ = runStateT p_peg_ d
                definition = runStateT p_definition d
                selection = runStateT p_selection d
                expressionHs = runStateT p_expressionHs d
                expression = runStateT p_expression d
                nameLeaf_ = runStateT p_nameLeaf_ d
                nameLeaf = runStateT p_nameLeaf d
                pat = runStateT p_pat d
                charLit = runStateT p_charLit d
                stringLit = runStateT p_stringLit d
                dq = runStateT p_dq d
                pats = runStateT p_pats d
                leaf = runStateT p_leaf d
                test = runStateT p_test d
                hsExp = runStateT p_hsExp d
                typ = runStateT p_typ d
                variable = runStateT p_variable d
                tvtail = runStateT p_tvtail d
                alpha = runStateT p_alpha d
                upper = runStateT p_upper d
                lower = runStateT p_lower d
                digit = runStateT p_digit d
                spaces = runStateT p_spaces d
                space = runStateT p_space d
                notNLString = runStateT p_notNLString d
                nl = runStateT p_nl d
                comment = runStateT p_comment d
                comments = runStateT p_comments d
                notComStr = runStateT p_notComStr d
                comEnd = runStateT p_comEnd d
                char = flip runStateT d (case getToken s of
                                             Just (c, s') -> do put (parse s')
                                                                return c
                                             _ -> throwError (strMsg "eof"))
dv_pragmaM :: PackratM MaybeString
dv_pragmaStrM :: PackratM String
dv_pragmaEndM :: PackratM Nil
dv_moduleDecM :: PackratM MaybeString
dv_moduleDecStrM :: PackratM String
dv_whrM :: PackratM Nil
dv_preImpPapM :: PackratM String
dv_prePegM :: PackratM String
dv_afterPegM :: PackratM String
dv_importPapillonM :: PackratM Nil
dv_varTokenM :: PackratM String
dv_typTokenM :: PackratM String
dv_papM :: PackratM Nil
dv_pegM :: PackratM TTPeg
dv_sourceTypeM :: PackratM String
dv_peg_M :: PackratM Peg
dv_definitionM :: PackratM Definition
dv_selectionM :: PackratM Selection
dv_expressionHsM :: PackratM ExpressionHs
dv_expressionM :: PackratM Expression
dv_nameLeaf_M :: PackratM NameLeaf_
dv_nameLeafM :: PackratM NameLeaf
dv_patM :: PackratM PatQ
dv_charLitM :: PackratM Char
dv_stringLitM :: PackratM String
dv_dqM :: PackratM Nil
dv_patsM :: PackratM PatQs
dv_leafM :: PackratM Leaf
dv_testM :: PackratM ExR
dv_hsExpM :: PackratM Ex
dv_typM :: PackratM String
dv_variableM :: PackratM String
dv_tvtailM :: PackratM String
dv_alphaM :: PackratM Char
dv_upperM :: PackratM Char
dv_lowerM :: PackratM Char
dv_digitM :: PackratM Char
dv_spacesM :: PackratM Nil
dv_spaceM :: PackratM Nil
dv_notNLStringM :: PackratM String
dv_nlM :: PackratM Nil
dv_commentM :: PackratM Nil
dv_commentsM :: PackratM Nil
dv_notComStrM :: PackratM Nil
dv_comEndM :: PackratM Nil
dv_pragmaM = StateT dv_pragma
dv_pragmaStrM = StateT dv_pragmaStr
dv_pragmaEndM = StateT dv_pragmaEnd
dv_moduleDecM = StateT dv_moduleDec
dv_moduleDecStrM = StateT dv_moduleDecStr
dv_whrM = StateT dv_whr
dv_preImpPapM = StateT dv_preImpPap
dv_prePegM = StateT dv_prePeg
dv_afterPegM = StateT dv_afterPeg
dv_importPapillonM = StateT dv_importPapillon
dv_varTokenM = StateT dv_varToken
dv_typTokenM = StateT dv_typToken
dv_papM = StateT dv_pap
dv_pegM = StateT dv_peg
dv_sourceTypeM = StateT dv_sourceType
dv_peg_M = StateT dv_peg_
dv_definitionM = StateT dv_definition
dv_selectionM = StateT dv_selection
dv_expressionHsM = StateT dv_expressionHs
dv_expressionM = StateT dv_expression
dv_nameLeaf_M = StateT dv_nameLeaf_
dv_nameLeafM = StateT dv_nameLeaf
dv_patM = StateT dv_pat
dv_charLitM = StateT dv_charLit
dv_stringLitM = StateT dv_stringLit
dv_dqM = StateT dv_dq
dv_patsM = StateT dv_pats
dv_leafM = StateT dv_leaf
dv_testM = StateT dv_test
dv_hsExpM = StateT dv_hsExp
dv_typM = StateT dv_typ
dv_variableM = StateT dv_variable
dv_tvtailM = StateT dv_tvtail
dv_alphaM = StateT dv_alpha
dv_upperM = StateT dv_upper
dv_lowerM = StateT dv_lower
dv_digitM = StateT dv_digit
dv_spacesM = StateT dv_spaces
dv_spaceM = StateT dv_space
dv_notNLStringM = StateT dv_notNLString
dv_nlM = StateT dv_nl
dv_commentM = StateT dv_comment
dv_commentsM = StateT dv_comments
dv_notComStrM = StateT dv_notComStr
dv_comEndM = StateT dv_comEnd
dvCharsM :: PackratM (Token String)
dvCharsM = StateT dvChars
p_pegFile :: PackratM PegFile
p_pragma :: PackratM MaybeString
p_pragmaStr :: PackratM String
p_pragmaEnd :: PackratM Nil
p_moduleDec :: PackratM MaybeString
p_moduleDecStr :: PackratM String
p_whr :: PackratM Nil
p_preImpPap :: PackratM String
p_prePeg :: PackratM String
p_afterPeg :: PackratM String
p_importPapillon :: PackratM Nil
p_varToken :: PackratM String
p_typToken :: PackratM String
p_pap :: PackratM Nil
p_peg :: PackratM TTPeg
p_sourceType :: PackratM String
p_peg_ :: PackratM Peg
p_definition :: PackratM Definition
p_selection :: PackratM Selection
p_expressionHs :: PackratM ExpressionHs
p_expression :: PackratM Expression
p_nameLeaf_ :: PackratM NameLeaf_
p_nameLeaf :: PackratM NameLeaf
p_pat :: PackratM PatQ
p_charLit :: PackratM Char
p_stringLit :: PackratM String
p_dq :: PackratM Nil
p_pats :: PackratM PatQs
p_leaf :: PackratM Leaf
p_test :: PackratM ExR
p_hsExp :: PackratM Ex
p_typ :: PackratM String
p_variable :: PackratM String
p_tvtail :: PackratM String
p_alpha :: PackratM Char
p_upper :: PackratM Char
p_lower :: PackratM Char
p_digit :: PackratM Char
p_spaces :: PackratM Nil
p_space :: PackratM Nil
p_notNLString :: PackratM String
p_nl :: PackratM Nil
p_comment :: PackratM Nil
p_comments :: PackratM Nil
p_notComStr :: PackratM Nil
p_comEnd :: PackratM Nil
p_pegFile = foldl1 mplus [do pr <- dv_pragmaM
                             return ()
                             if True then return () else throwError (strMsg "not match")
                             md <- dv_moduleDecM
                             return ()
                             if True then return () else throwError (strMsg "not match")
                             pip <- dv_preImpPapM
                             return ()
                             if True then return () else throwError (strMsg "not match")
                             dv_importPapillonM >> return ()
                             if True then return () else throwError (strMsg "not match")
                             pp <- dv_prePegM
                             return ()
                             if True then return () else throwError (strMsg "not match")
                             dv_papM >> return ()
                             if True then return () else throwError (strMsg "not match")
                             p <- dv_pegM
                             return ()
                             if True then return () else throwError (strMsg "not match")
                             dv_spacesM >> return ()
                             if True then return () else throwError (strMsg "not match")
                             xx0_0 <- dvCharsM
                             case xx0_0 of
                                 '|' -> return ()
                                 _ -> throwError (strMsg "not match")
                             let '|' = xx0_0
                             return ()
                             if True then return () else throwError (strMsg "not match")
                             xx1_1 <- dvCharsM
                             case xx1_1 of
                                 ']' -> return ()
                                 _ -> throwError (strMsg "not match")
                             let ']' = xx1_1
                             return ()
                             if True then return () else throwError (strMsg "not match")
                             xx2_2 <- dvCharsM
                             case xx2_2 of
                                 '\n' -> return ()
                                 _ -> throwError (strMsg "not match")
                             let '\n' = xx2_2
                             return ()
                             if True then return () else throwError (strMsg "not match")
                             atp <- dv_afterPegM
                             return ()
                             if True then return () else throwError (strMsg "not match")
                             return (id mkPegFile pr md pip pp p atp),
                          do pr <- dv_pragmaM
                             return ()
                             if True then return () else throwError (strMsg "not match")
                             md <- dv_moduleDecM
                             return ()
                             if True then return () else throwError (strMsg "not match")
                             pp <- dv_prePegM
                             return ()
                             if True then return () else throwError (strMsg "not match")
                             dv_papM >> return ()
                             if True then return () else throwError (strMsg "not match")
                             p <- dv_pegM
                             return ()
                             if True then return () else throwError (strMsg "not match")
                             dv_spacesM >> return ()
                             if True then return () else throwError (strMsg "not match")
                             xx3_3 <- dvCharsM
                             case xx3_3 of
                                 '|' -> return ()
                                 _ -> throwError (strMsg "not match")
                             let '|' = xx3_3
                             return ()
                             if True then return () else throwError (strMsg "not match")
                             xx4_4 <- dvCharsM
                             case xx4_4 of
                                 ']' -> return ()
                                 _ -> throwError (strMsg "not match")
                             let ']' = xx4_4
                             return ()
                             if True then return () else throwError (strMsg "not match")
                             xx5_5 <- dvCharsM
                             case xx5_5 of
                                 '\n' -> return ()
                                 _ -> throwError (strMsg "not match")
                             let '\n' = xx5_5
                             return ()
                             if True then return () else throwError (strMsg "not match")
                             atp <- dv_afterPegM
                             return ()
                             if True then return () else throwError (strMsg "not match")
                             return (id mkPegFile pr md emp pp p atp)]
p_pragma = foldl1 mplus [do dv_spacesM >> return ()
                            if True then return () else throwError (strMsg "not match")
                            xx6_6 <- dvCharsM
                            case xx6_6 of
                                '{' -> return ()
                                _ -> throwError (strMsg "not match")
                            let '{' = xx6_6
                            return ()
                            if True then return () else throwError (strMsg "not match")
                            xx7_7 <- dvCharsM
                            case xx7_7 of
                                '-' -> return ()
                                _ -> throwError (strMsg "not match")
                            let '-' = xx7_7
                            return ()
                            if True then return () else throwError (strMsg "not match")
                            xx8_8 <- dvCharsM
                            case xx8_8 of
                                '#' -> return ()
                                _ -> throwError (strMsg "not match")
                            let '#' = xx8_8
                            return ()
                            if True then return () else throwError (strMsg "not match")
                            s <- dv_pragmaStrM
                            return ()
                            if True then return () else throwError (strMsg "not match")
                            dv_pragmaEndM >> return ()
                            if True then return () else throwError (strMsg "not match")
                            dv_spacesM >> return ()
                            if True then return () else throwError (strMsg "not match")
                            return (id just s),
                         do dv_spacesM >> return ()
                            if True then return () else throwError (strMsg "not match")
                            return (id nothing)]
p_pragmaStr = foldl1 mplus [do ddd9_9 <- get
                               flipMaybe (do dv_pragmaEndM >> return ()
                                             if True
                                              then return ()
                                              else throwError (strMsg "not match"))
                               put ddd9_9
                               xx10_10 <- dvCharsM
                               let c = xx10_10
                               if True then return () else throwError (strMsg "not match")
                               s <- dv_pragmaStrM
                               return ()
                               if True then return () else throwError (strMsg "not match")
                               return (id cons c s),
                            do return (id emp)]
p_pragmaEnd = foldl1 mplus [do xx11_11 <- dvCharsM
                               case xx11_11 of
                                   '#' -> return ()
                                   _ -> throwError (strMsg "not match")
                               let '#' = xx11_11
                               return ()
                               if True then return () else throwError (strMsg "not match")
                               xx12_12 <- dvCharsM
                               case xx12_12 of
                                   '-' -> return ()
                                   _ -> throwError (strMsg "not match")
                               let '-' = xx12_12
                               return ()
                               if True then return () else throwError (strMsg "not match")
                               xx13_13 <- dvCharsM
                               case xx13_13 of
                                   '}' -> return ()
                                   _ -> throwError (strMsg "not match")
                               let '}' = xx13_13
                               return ()
                               if True then return () else throwError (strMsg "not match")
                               return (id nil)]
p_moduleDec = foldl1 mplus [do xx14_14 <- dvCharsM
                               case xx14_14 of
                                   'm' -> return ()
                                   _ -> throwError (strMsg "not match")
                               let 'm' = xx14_14
                               return ()
                               if True then return () else throwError (strMsg "not match")
                               xx15_15 <- dvCharsM
                               case xx15_15 of
                                   'o' -> return ()
                                   _ -> throwError (strMsg "not match")
                               let 'o' = xx15_15
                               return ()
                               if True then return () else throwError (strMsg "not match")
                               xx16_16 <- dvCharsM
                               case xx16_16 of
                                   'd' -> return ()
                                   _ -> throwError (strMsg "not match")
                               let 'd' = xx16_16
                               return ()
                               if True then return () else throwError (strMsg "not match")
                               xx17_17 <- dvCharsM
                               case xx17_17 of
                                   'u' -> return ()
                                   _ -> throwError (strMsg "not match")
                               let 'u' = xx17_17
                               return ()
                               if True then return () else throwError (strMsg "not match")
                               xx18_18 <- dvCharsM
                               case xx18_18 of
                                   'l' -> return ()
                                   _ -> throwError (strMsg "not match")
                               let 'l' = xx18_18
                               return ()
                               if True then return () else throwError (strMsg "not match")
                               xx19_19 <- dvCharsM
                               case xx19_19 of
                                   'e' -> return ()
                                   _ -> throwError (strMsg "not match")
                               let 'e' = xx19_19
                               return ()
                               if True then return () else throwError (strMsg "not match")
                               s <- dv_moduleDecStrM
                               return ()
                               if True then return () else throwError (strMsg "not match")
                               dv_whrM >> return ()
                               if True then return () else throwError (strMsg "not match")
                               return (id just s),
                            do return (id nothing)]
p_moduleDecStr = foldl1 mplus [do ddd20_20 <- get
                                  flipMaybe (do dv_whrM >> return ()
                                                if True
                                                 then return ()
                                                 else throwError (strMsg "not match"))
                                  put ddd20_20
                                  xx21_21 <- dvCharsM
                                  let c = xx21_21
                                  if True then return () else throwError (strMsg "not match")
                                  s <- dv_moduleDecStrM
                                  return ()
                                  if True then return () else throwError (strMsg "not match")
                                  return (id cons c s),
                               do return (id emp)]
p_whr = foldl1 mplus [do xx22_22 <- dvCharsM
                         case xx22_22 of
                             'w' -> return ()
                             _ -> throwError (strMsg "not match")
                         let 'w' = xx22_22
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         xx23_23 <- dvCharsM
                         case xx23_23 of
                             'h' -> return ()
                             _ -> throwError (strMsg "not match")
                         let 'h' = xx23_23
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         xx24_24 <- dvCharsM
                         case xx24_24 of
                             'e' -> return ()
                             _ -> throwError (strMsg "not match")
                         let 'e' = xx24_24
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         xx25_25 <- dvCharsM
                         case xx25_25 of
                             'r' -> return ()
                             _ -> throwError (strMsg "not match")
                         let 'r' = xx25_25
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         xx26_26 <- dvCharsM
                         case xx26_26 of
                             'e' -> return ()
                             _ -> throwError (strMsg "not match")
                         let 'e' = xx26_26
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         return (id nil)]
p_preImpPap = foldl1 mplus [do ddd27_27 <- get
                               flipMaybe (do dv_importPapillonM >> return ()
                                             if True
                                              then return ()
                                              else throwError (strMsg "not match"))
                               put ddd27_27
                               ddd28_28 <- get
                               flipMaybe (do dv_papM >> return ()
                                             if True
                                              then return ()
                                              else throwError (strMsg "not match"))
                               put ddd28_28
                               xx29_29 <- dvCharsM
                               let c = xx29_29
                               if True then return () else throwError (strMsg "not match")
                               pip <- dv_preImpPapM
                               return ()
                               if True then return () else throwError (strMsg "not match")
                               return (id cons c pip),
                            do return (id emp)]
p_prePeg = foldl1 mplus [do ddd30_30 <- get
                            flipMaybe (do dv_papM >> return ()
                                          if True
                                           then return ()
                                           else throwError (strMsg "not match"))
                            put ddd30_30
                            xx31_31 <- dvCharsM
                            let c = xx31_31
                            if True then return () else throwError (strMsg "not match")
                            pp <- dv_prePegM
                            return ()
                            if True then return () else throwError (strMsg "not match")
                            return (id cons c pp),
                         do return (id emp)]
p_afterPeg = foldl1 mplus [do xx32_32 <- dvCharsM
                              let c = xx32_32
                              if True then return () else throwError (strMsg "not match")
                              atp <- dv_afterPegM
                              return ()
                              if True then return () else throwError (strMsg "not match")
                              return (id cons c atp),
                           do return (id emp)]
p_importPapillon = foldl1 mplus [do xx33_33 <- dv_varTokenM
                                    case xx33_33 of
                                        "import" -> return ()
                                        _ -> throwError (strMsg "not match")
                                    let "import" = xx33_33
                                    return ()
                                    if True then return () else throwError (strMsg "not match")
                                    xx34_34 <- dv_typTokenM
                                    case xx34_34 of
                                        "Text" -> return ()
                                        _ -> throwError (strMsg "not match")
                                    let "Text" = xx34_34
                                    return ()
                                    if True then return () else throwError (strMsg "not match")
                                    xx35_35 <- dvCharsM
                                    case xx35_35 of
                                        '.' -> return ()
                                        _ -> throwError (strMsg "not match")
                                    let '.' = xx35_35
                                    return ()
                                    if True then return () else throwError (strMsg "not match")
                                    dv_spacesM >> return ()
                                    if True then return () else throwError (strMsg "not match")
                                    xx36_36 <- dv_typTokenM
                                    case xx36_36 of
                                        "Papillon" -> return ()
                                        _ -> throwError (strMsg "not match")
                                    let "Papillon" = xx36_36
                                    return ()
                                    if True then return () else throwError (strMsg "not match")
                                    return (id nil)]
p_varToken = foldl1 mplus [do v <- dv_variableM
                              return ()
                              if True then return () else throwError (strMsg "not match")
                              dv_spacesM >> return ()
                              if True then return () else throwError (strMsg "not match")
                              return (id v)]
p_typToken = foldl1 mplus [do t <- dv_typM
                              return ()
                              if True then return () else throwError (strMsg "not match")
                              dv_spacesM >> return ()
                              if True then return () else throwError (strMsg "not match")
                              return (id t)]
p_pap = foldl1 mplus [do xx37_37 <- dvCharsM
                         case xx37_37 of
                             '\n' -> return ()
                             _ -> throwError (strMsg "not match")
                         let '\n' = xx37_37
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         xx38_38 <- dvCharsM
                         case xx38_38 of
                             '[' -> return ()
                             _ -> throwError (strMsg "not match")
                         let '[' = xx38_38
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         xx39_39 <- dvCharsM
                         case xx39_39 of
                             'p' -> return ()
                             _ -> throwError (strMsg "not match")
                         let 'p' = xx39_39
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         xx40_40 <- dvCharsM
                         case xx40_40 of
                             'a' -> return ()
                             _ -> throwError (strMsg "not match")
                         let 'a' = xx40_40
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         xx41_41 <- dvCharsM
                         case xx41_41 of
                             'p' -> return ()
                             _ -> throwError (strMsg "not match")
                         let 'p' = xx41_41
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         xx42_42 <- dvCharsM
                         case xx42_42 of
                             'i' -> return ()
                             _ -> throwError (strMsg "not match")
                         let 'i' = xx42_42
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         xx43_43 <- dvCharsM
                         case xx43_43 of
                             'l' -> return ()
                             _ -> throwError (strMsg "not match")
                         let 'l' = xx43_43
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         xx44_44 <- dvCharsM
                         case xx44_44 of
                             'l' -> return ()
                             _ -> throwError (strMsg "not match")
                         let 'l' = xx44_44
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         xx45_45 <- dvCharsM
                         case xx45_45 of
                             'o' -> return ()
                             _ -> throwError (strMsg "not match")
                         let 'o' = xx45_45
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         xx46_46 <- dvCharsM
                         case xx46_46 of
                             'n' -> return ()
                             _ -> throwError (strMsg "not match")
                         let 'n' = xx46_46
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         xx47_47 <- dvCharsM
                         case xx47_47 of
                             '|' -> return ()
                             _ -> throwError (strMsg "not match")
                         let '|' = xx47_47
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         xx48_48 <- dvCharsM
                         case xx48_48 of
                             '\n' -> return ()
                             _ -> throwError (strMsg "not match")
                         let '\n' = xx48_48
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         return (id nil)]
p_peg = foldl1 mplus [do dv_spacesM >> return ()
                         if True then return () else throwError (strMsg "not match")
                         s <- dv_sourceTypeM
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         p <- dv_peg_M
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         return (id mkTTPeg s p),
                      do p <- dv_peg_M
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         return (id mkTTPeg tString p)]
p_sourceType = foldl1 mplus [do xx49_49 <- dv_varTokenM
                                case xx49_49 of
                                    "source" -> return ()
                                    _ -> throwError (strMsg "not match")
                                let "source" = xx49_49
                                return ()
                                if True then return () else throwError (strMsg "not match")
                                xx50_50 <- dvCharsM
                                case xx50_50 of
                                    ':' -> return ()
                                    _ -> throwError (strMsg "not match")
                                let ':' = xx50_50
                                return ()
                                if True then return () else throwError (strMsg "not match")
                                dv_spacesM >> return ()
                                if True then return () else throwError (strMsg "not match")
                                v <- dv_typTokenM
                                return ()
                                if True then return () else throwError (strMsg "not match")
                                return (id v)]
p_peg_ = foldl1 mplus [do dv_spacesM >> return ()
                          if True then return () else throwError (strMsg "not match")
                          d <- dv_definitionM
                          return ()
                          if True then return () else throwError (strMsg "not match")
                          p <- dv_peg_M
                          return ()
                          if True then return () else throwError (strMsg "not match")
                          return (id cons d p),
                       do return (id emp)]
p_definition = foldl1 mplus [do v <- dv_variableM
                                return ()
                                if True then return () else throwError (strMsg "not match")
                                dv_spacesM >> return ()
                                if True then return () else throwError (strMsg "not match")
                                xx51_51 <- dvCharsM
                                case xx51_51 of
                                    ':' -> return ()
                                    _ -> throwError (strMsg "not match")
                                let ':' = xx51_51
                                return ()
                                if True then return () else throwError (strMsg "not match")
                                xx52_52 <- dvCharsM
                                case xx52_52 of
                                    ':' -> return ()
                                    _ -> throwError (strMsg "not match")
                                let ':' = xx52_52
                                return ()
                                if True then return () else throwError (strMsg "not match")
                                dv_spacesM >> return ()
                                if True then return () else throwError (strMsg "not match")
                                t <- dv_typM
                                return ()
                                if True then return () else throwError (strMsg "not match")
                                dv_spacesM >> return ()
                                if True then return () else throwError (strMsg "not match")
                                xx53_53 <- dvCharsM
                                case xx53_53 of
                                    '=' -> return ()
                                    _ -> throwError (strMsg "not match")
                                let '=' = xx53_53
                                return ()
                                if True then return () else throwError (strMsg "not match")
                                dv_spacesM >> return ()
                                if True then return () else throwError (strMsg "not match")
                                sel <- dv_selectionM
                                return ()
                                if True then return () else throwError (strMsg "not match")
                                dv_spacesM >> return ()
                                if True then return () else throwError (strMsg "not match")
                                xx54_54 <- dvCharsM
                                case xx54_54 of
                                    ';' -> return ()
                                    _ -> throwError (strMsg "not match")
                                let ';' = xx54_54
                                return ()
                                if True then return () else throwError (strMsg "not match")
                                return (id mkDef v t sel)]
p_selection = foldl1 mplus [do ex <- dv_expressionHsM
                               return ()
                               if True then return () else throwError (strMsg "not match")
                               dv_spacesM >> return ()
                               if True then return () else throwError (strMsg "not match")
                               xx55_55 <- dvCharsM
                               case xx55_55 of
                                   '/' -> return ()
                                   _ -> throwError (strMsg "not match")
                               let '/' = xx55_55
                               return ()
                               if True then return () else throwError (strMsg "not match")
                               dv_spacesM >> return ()
                               if True then return () else throwError (strMsg "not match")
                               sel <- dv_selectionM
                               return ()
                               if True then return () else throwError (strMsg "not match")
                               return (id cons ex sel),
                            do ex <- dv_expressionHsM
                               return ()
                               if True then return () else throwError (strMsg "not match")
                               return (id cons ex emp)]
p_expressionHs = foldl1 mplus [do e <- dv_expressionM
                                  return ()
                                  if True then return () else throwError (strMsg "not match")
                                  dv_spacesM >> return ()
                                  if True then return () else throwError (strMsg "not match")
                                  xx56_56 <- dvCharsM
                                  case xx56_56 of
                                      '{' -> return ()
                                      _ -> throwError (strMsg "not match")
                                  let '{' = xx56_56
                                  return ()
                                  if True then return () else throwError (strMsg "not match")
                                  dv_spacesM >> return ()
                                  if True then return () else throwError (strMsg "not match")
                                  h <- dv_hsExpM
                                  return ()
                                  if True then return () else throwError (strMsg "not match")
                                  dv_spacesM >> return ()
                                  if True then return () else throwError (strMsg "not match")
                                  xx57_57 <- dvCharsM
                                  case xx57_57 of
                                      '}' -> return ()
                                      _ -> throwError (strMsg "not match")
                                  let '}' = xx57_57
                                  return ()
                                  if True then return () else throwError (strMsg "not match")
                                  return (id mkExpressionHs e h)]
p_expression = foldl1 mplus [do l <- dv_nameLeaf_M
                                return ()
                                if True then return () else throwError (strMsg "not match")
                                dv_spacesM >> return ()
                                if True then return () else throwError (strMsg "not match")
                                e <- dv_expressionM
                                return ()
                                if True then return () else throwError (strMsg "not match")
                                return (id cons l e),
                             do return (id emp)]
p_nameLeaf_ = foldl1 mplus [do xx58_58 <- dvCharsM
                               case xx58_58 of
                                   '!' -> return ()
                                   _ -> throwError (strMsg "not match")
                               let '!' = xx58_58
                               return ()
                               if True then return () else throwError (strMsg "not match")
                               nl <- dv_nameLeafM
                               return ()
                               if True then return () else throwError (strMsg "not match")
                               return (id notAfter nl),
                            do nl <- dv_nameLeafM
                               return ()
                               if True then return () else throwError (strMsg "not match")
                               return (id here nl)]
p_nameLeaf = foldl1 mplus [do n <- dv_patM
                              return ()
                              if True then return () else throwError (strMsg "not match")
                              xx59_59 <- dvCharsM
                              case xx59_59 of
                                  ':' -> return ()
                                  _ -> throwError (strMsg "not match")
                              let ':' = xx59_59
                              return ()
                              if True then return () else throwError (strMsg "not match")
                              xx60_60 <- dvCharsM
                              let o = xx60_60
                              if id isOpen o then return () else throwError (strMsg "not match")
                              ex <- dv_selectionM
                              return ()
                              if True then return () else throwError (strMsg "not match")
                              xx61_61 <- dvCharsM
                              let c = xx61_61
                              if id isClose c then return () else throwError (strMsg "not match")
                              xx62_62 <- dvCharsM
                              let k = xx62_62
                              if id isKome k then return () else throwError (strMsg "not match")
                              dv_spacesM >> return ()
                              if True then return () else throwError (strMsg "not match")
                              return (id mkNameLeafList n ex),
                           do xx63_63 <- dvCharsM
                              let o = xx63_63
                              if id isOpen o then return () else throwError (strMsg "not match")
                              nl <- dv_nameLeafM
                              return ()
                              if True then return () else throwError (strMsg "not match")
                              xx64_64 <- dvCharsM
                              let c = xx64_64
                              if id isClose c then return () else throwError (strMsg "not match")
                              return (id nl),
                           do n <- dv_patM
                              return ()
                              if True then return () else throwError (strMsg "not match")
                              xx65_65 <- dvCharsM
                              case xx65_65 of
                                  ':' -> return ()
                                  _ -> throwError (strMsg "not match")
                              let ':' = xx65_65
                              return ()
                              if True then return () else throwError (strMsg "not match")
                              l <- dv_leafM
                              return ()
                              if True then return () else throwError (strMsg "not match")
                              return (id mkNameLeaf n l),
                           do n <- dv_patM
                              return ()
                              if True then return () else throwError (strMsg "not match")
                              return (id mkNameLeaf n ctLeaf)]
p_pat = foldl1 mplus [do xx66_66 <- dv_variableM
                         case xx66_66 of
                             "_" -> return ()
                             _ -> throwError (strMsg "not match")
                         let "_" = xx66_66
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         return (id wildP),
                      do n <- dv_variableM
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         return (id strToPatQ n),
                      do t <- dv_typM
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         dv_spacesM >> return ()
                         if True then return () else throwError (strMsg "not match")
                         ps <- dv_patsM
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         return (id conToPatQ t ps),
                      do xx67_67 <- dvCharsM
                         case xx67_67 of
                             '\'' -> return ()
                             _ -> throwError (strMsg "not match")
                         let '\'' = xx67_67
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         c <- dv_charLitM
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         xx68_68 <- dvCharsM
                         case xx68_68 of
                             '\'' -> return ()
                             _ -> throwError (strMsg "not match")
                         let '\'' = xx68_68
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         return (id charP c),
                      do xx69_69 <- dvCharsM
                         case xx69_69 of
                             '"' -> return ()
                             _ -> throwError (strMsg "not match")
                         let '"' = xx69_69
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         s <- dv_stringLitM
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         xx70_70 <- dvCharsM
                         case xx70_70 of
                             '"' -> return ()
                             _ -> throwError (strMsg "not match")
                         let '"' = xx70_70
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         return (id stringP s)]
p_charLit = foldl1 mplus [do xx71_71 <- dvCharsM
                             let c = xx71_71
                             if id isAlphaNumOt c
                              then return ()
                              else throwError (strMsg "not match")
                             return (id c),
                          do xx72_72 <- dvCharsM
                             case xx72_72 of
                                 '\\' -> return ()
                                 _ -> throwError (strMsg "not match")
                             let '\\' = xx72_72
                             return ()
                             if True then return () else throwError (strMsg "not match")
                             xx73_73 <- dvCharsM
                             let c = xx73_73
                             if id elemNTs c then return () else throwError (strMsg "not match")
                             return (id getNTs c)]
p_stringLit = foldl1 mplus [do ddd74_74 <- get
                               flipMaybe (do dv_dqM >> return ()
                                             if True
                                              then return ()
                                              else throwError (strMsg "not match"))
                               put ddd74_74
                               xx75_75 <- dvCharsM
                               let c = xx75_75
                               if True then return () else throwError (strMsg "not match")
                               s <- dv_stringLitM
                               return ()
                               if True then return () else throwError (strMsg "not match")
                               return (id cons c s),
                            do return (id emp)]
p_dq = foldl1 mplus [do xx76_76 <- dvCharsM
                        case xx76_76 of
                            '"' -> return ()
                            _ -> throwError (strMsg "not match")
                        let '"' = xx76_76
                        return ()
                        if True then return () else throwError (strMsg "not match")
                        return (id nil)]
p_pats = foldl1 mplus [do p <- dv_patM
                          return ()
                          if True then return () else throwError (strMsg "not match")
                          ps <- dv_patsM
                          return ()
                          if True then return () else throwError (strMsg "not match")
                          return (id cons p ps),
                       do return (id emp)]
p_leaf = foldl1 mplus [do t <- dv_testM
                          return ()
                          if True then return () else throwError (strMsg "not match")
                          return (id boolLeaf t),
                       do v <- dv_variableM
                          return ()
                          if True then return () else throwError (strMsg "not match")
                          t <- dv_testM
                          return ()
                          if True then return () else throwError (strMsg "not match")
                          return (id ruleLeaf v t),
                       do v <- dv_variableM
                          return ()
                          if True then return () else throwError (strMsg "not match")
                          return (id ruleLeaf v true)]
p_test = foldl1 mplus [do xx77_77 <- dvCharsM
                          case xx77_77 of
                              '[' -> return ()
                              _ -> throwError (strMsg "not match")
                          let '[' = xx77_77
                          return ()
                          if True then return () else throwError (strMsg "not match")
                          h <- dv_hsExpM
                          return ()
                          if True then return () else throwError (strMsg "not match")
                          xx78_78 <- dvCharsM
                          case xx78_78 of
                              ']' -> return ()
                              _ -> throwError (strMsg "not match")
                          let ']' = xx78_78
                          return ()
                          if True then return () else throwError (strMsg "not match")
                          return (id getEx h)]
p_hsExp = foldl1 mplus [do v <- dv_variableM
                           return ()
                           if True then return () else throwError (strMsg "not match")
                           dv_spacesM >> return ()
                           if True then return () else throwError (strMsg "not match")
                           h <- dv_hsExpM
                           return ()
                           if True then return () else throwError (strMsg "not match")
                           return (id apply v h),
                        do v <- dv_variableM
                           return ()
                           if True then return () else throwError (strMsg "not match")
                           return (id toExp v)]
p_typ = foldl1 mplus [do u <- dv_upperM
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         t <- dv_tvtailM
                         return ()
                         if True then return () else throwError (strMsg "not match")
                         return (id cons u t)]
p_variable = foldl1 mplus [do l <- dv_lowerM
                              return ()
                              if True then return () else throwError (strMsg "not match")
                              t <- dv_tvtailM
                              return ()
                              if True then return () else throwError (strMsg "not match")
                              return (id cons l t)]
p_tvtail = foldl1 mplus [do a <- dv_alphaM
                            return ()
                            if True then return () else throwError (strMsg "not match")
                            t <- dv_tvtailM
                            return ()
                            if True then return () else throwError (strMsg "not match")
                            return (id cons a t),
                         do return (id emp)]
p_alpha = foldl1 mplus [do u <- dv_upperM
                           return ()
                           if True then return () else throwError (strMsg "not match")
                           return (id u),
                        do l <- dv_lowerM
                           return ()
                           if True then return () else throwError (strMsg "not match")
                           return (id l),
                        do d <- dv_digitM
                           return ()
                           if True then return () else throwError (strMsg "not match")
                           return (id d)]
p_upper = foldl1 mplus [do xx79_79 <- dvCharsM
                           let u = xx79_79
                           if id isUpper u then return () else throwError (strMsg "not match")
                           return (id u)]
p_lower = foldl1 mplus [do xx80_80 <- dvCharsM
                           let l = xx80_80
                           if id isLowerU l
                            then return ()
                            else throwError (strMsg "not match")
                           return (id l)]
p_digit = foldl1 mplus [do xx81_81 <- dvCharsM
                           let d = xx81_81
                           if id isDigit d then return () else throwError (strMsg "not match")
                           return (id d)]
p_spaces = foldl1 mplus [do dv_spaceM >> return ()
                            if True then return () else throwError (strMsg "not match")
                            dv_spacesM >> return ()
                            if True then return () else throwError (strMsg "not match")
                            return (id nil),
                         do return (id nil)]
p_space = foldl1 mplus [do xx82_82 <- dvCharsM
                           let s = xx82_82
                           if id isSpace s then return () else throwError (strMsg "not match")
                           return (id nil),
                        do xx83_83 <- dvCharsM
                           case xx83_83 of
                               '-' -> return ()
                               _ -> throwError (strMsg "not match")
                           let '-' = xx83_83
                           return ()
                           if True then return () else throwError (strMsg "not match")
                           xx84_84 <- dvCharsM
                           case xx84_84 of
                               '-' -> return ()
                               _ -> throwError (strMsg "not match")
                           let '-' = xx84_84
                           return ()
                           if True then return () else throwError (strMsg "not match")
                           dv_notNLStringM >> return ()
                           if True then return () else throwError (strMsg "not match")
                           dv_nlM >> return ()
                           if True then return () else throwError (strMsg "not match")
                           return (id nil),
                        do dv_commentM >> return ()
                           if True then return () else throwError (strMsg "not match")
                           return (id nil)]
p_notNLString = foldl1 mplus [do ddd85_85 <- get
                                 flipMaybe (do dv_nlM >> return ()
                                               if True
                                                then return ()
                                                else throwError (strMsg "not match"))
                                 put ddd85_85
                                 xx86_86 <- dvCharsM
                                 let c = xx86_86
                                 if True then return () else throwError (strMsg "not match")
                                 s <- dv_notNLStringM
                                 return ()
                                 if True then return () else throwError (strMsg "not match")
                                 return (id cons c s),
                              do return (id emp)]
p_nl = foldl1 mplus [do xx87_87 <- dvCharsM
                        case xx87_87 of
                            '\n' -> return ()
                            _ -> throwError (strMsg "not match")
                        let '\n' = xx87_87
                        return ()
                        if True then return () else throwError (strMsg "not match")
                        return (id nil)]
p_comment = foldl1 mplus [do xx88_88 <- dvCharsM
                             case xx88_88 of
                                 '{' -> return ()
                                 _ -> throwError (strMsg "not match")
                             let '{' = xx88_88
                             return ()
                             if True then return () else throwError (strMsg "not match")
                             xx89_89 <- dvCharsM
                             case xx89_89 of
                                 '-' -> return ()
                                 _ -> throwError (strMsg "not match")
                             let '-' = xx89_89
                             return ()
                             if True then return () else throwError (strMsg "not match")
                             ddd90_90 <- get
                             flipMaybe (do xx91_91 <- dvCharsM
                                           case xx91_91 of
                                               '#' -> return ()
                                               _ -> throwError (strMsg "not match")
                                           let '#' = xx91_91
                                           return ()
                                           if True
                                            then return ()
                                            else throwError (strMsg "not match"))
                             put ddd90_90
                             dv_commentsM >> return ()
                             if True then return () else throwError (strMsg "not match")
                             dv_comEndM >> return ()
                             if True then return () else throwError (strMsg "not match")
                             return (id nil)]
p_comments = foldl1 mplus [do dv_notComStrM >> return ()
                              if True then return () else throwError (strMsg "not match")
                              dv_commentM >> return ()
                              if True then return () else throwError (strMsg "not match")
                              dv_commentsM >> return ()
                              if True then return () else throwError (strMsg "not match")
                              return (id nil),
                           do dv_notComStrM >> return ()
                              if True then return () else throwError (strMsg "not match")
                              return (id nil)]
p_notComStr = foldl1 mplus [do ddd92_92 <- get
                               flipMaybe (do dv_commentM >> return ()
                                             if True
                                              then return ()
                                              else throwError (strMsg "not match"))
                               put ddd92_92
                               ddd93_93 <- get
                               flipMaybe (do dv_comEndM >> return ()
                                             if True
                                              then return ()
                                              else throwError (strMsg "not match"))
                               put ddd93_93
                               _ <- dvCharsM
                               if True then return () else throwError (strMsg "not match")
                               dv_notComStrM >> return ()
                               if True then return () else throwError (strMsg "not match")
                               return (id nil),
                            do return (id nil)]
p_comEnd = foldl1 mplus [do xx95_94 <- dvCharsM
                            case xx95_94 of
                                '-' -> return ()
                                _ -> throwError (strMsg "not match")
                            let '-' = xx95_94
                            return ()
                            if True then return () else throwError (strMsg "not match")
                            xx96_95 <- dvCharsM
                            case xx96_95 of
                                '}' -> return ()
                                _ -> throwError (strMsg "not match")
                            let '}' = xx96_95
                            return ()
                            if True then return () else throwError (strMsg "not match")
                            return (id nil)]

class Source sl
    where type Token sl
          getToken :: sl -> Maybe ((Token sl, sl))
class SourceList c
    where listToken :: [c] -> Maybe ((c, [c]))
instance SourceList Char
    where listToken (c : s) = Just (c, s)
          listToken _ = Nothing
instance SourceList c => Source ([c])
    where type Token ([c]) = c
          getToken = listToken
list :: forall m a . (MonadPlus m, Applicative m) => m a -> m ([a])
list1 :: forall m a . (MonadPlus m, Applicative m) =>
                      m a -> m ([a])
list p = list1 p `mplus` return []
list1 p = ((:) <$> p) <*> list p