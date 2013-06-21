{-# LANGUAGE FlexibleContexts, TemplateHaskell , FlexibleContexts, PackageImports, TypeFamilies #-}
module  Text.Papillon.Parser (
	Peg,
	Definition,
	ExpressionHs,
	NameLeaf,
	NameLeaf_(..),
	parse,
	dv_peg,
	dv_pegFile,
)  where
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Monad.Trans.Error (Error (..))



import Data.Char
import Language.Haskell.TH

type MaybeString = Maybe String

type Nil = ()
type Leaf = Either String ExR
type NameLeaf = (PatQ, Leaf)
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
ctLeaf = Right $ varE (mkName "const") `appE` conE (mkName "True")

left :: b -> Either a b
right :: a -> Either a b
left = Right
right = Left

just :: a -> Maybe a
just = Just
nothing :: Maybe a
nothing = Nothing

nil :: Nil
nil = ()

cons :: a -> [a] -> [a]
cons = (:)

type PatQs = [PatQ]

mkNameLeaf :: PatQ -> b -> (PatQ, b)
mkNameLeaf = (,)

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

empty :: [a]
empty = []

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
	", FlexibleContexts, PackageImports, TypeFamilies #-}\n"
addModules =
	"import \"monads-tf\" Control.Monad.State\n" ++
	"import \"monads-tf\" Control.Monad.Error\n" ++
	"import Control.Monad.Trans.Error (Error (..))\n"

true :: Bool
true = True

charP :: Char -> PatQ
charP = litP . charL
stringP :: String -> PatQ
stringP = litP . stringL

isAlphaNumOt, elemNTs :: Char -> Bool
isAlphaNumOt c = isAlphaNum c || c `elem` "{-#.\":}"
elemNTs = (`elem` "nt\\'")

getNTs :: Char -> Char
getNTs 'n' = '\n'
getNTs 't' = '\t'
getNTs '\\' = '\\'
getNTs '\'' = '\''
getNTs o = o

isEqual, isSlash, isSemi, isColon, isOpenWave, isCloseWave, isLowerU, isNot,
	isChon, isDQ, isBS :: Char -> Bool
isEqual = (== '=')
isSlash = (== '/')
isSemi = (== ';')
isColon = (== ':')
isOpenWave = (== '{')
isCloseWave = (== '}')
isLowerU c = isLower c || c == '_'
isNot = (== '!')
isChon = (== '\'')
isDQ = (== '"')
isBS = (== '\\')

isOpenBr, isP, isA, isI, isL, isO, isN, isBar, isCloseBr, isNL :: Char -> Bool
[isOpenBr, isP, isA, isI, isL, isO, isN, isBar, isCloseBr, isNL] =
	map (==) "[pailon|]\n"

{-
tString, tChar :: TypeQ
tString = varT ''String
tChar = varT ''Char
-}

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
                             md <- dv_moduleDecM
                             return ()
                             pip <- dv_preImpPapM
                             return ()
                             dv_importPapillonM >> return ()
                             pp <- dv_prePegM
                             return ()
                             dv_papM >> return ()
                             p <- dv_pegM
                             return ()
                             dv_spacesM >> return ()
                             xx0_0 <- dvCharsM
                             if id isBar xx0_0
                              then return ()
                              else throwError (strMsg "not match")
                             xx1_1 <- dvCharsM
                             if id isCloseBr xx1_1
                              then return ()
                              else throwError (strMsg "not match")
                             xx2_2 <- dvCharsM
                             if id isNL xx2_2
                              then return ()
                              else throwError (strMsg "not match")
                             atp <- dv_afterPegM
                             return ()
                             return (id mkPegFile pr md pip pp p atp),
                          do pr <- dv_pragmaM
                             return ()
                             md <- dv_moduleDecM
                             return ()
                             pp <- dv_prePegM
                             return ()
                             dv_papM >> return ()
                             p <- dv_pegM
                             return ()
                             dv_spacesM >> return ()
                             xx3_3 <- dvCharsM
                             if id isBar xx3_3
                              then return ()
                              else throwError (strMsg "not match")
                             xx4_4 <- dvCharsM
                             if id isCloseBr xx4_4
                              then return ()
                              else throwError (strMsg "not match")
                             xx5_5 <- dvCharsM
                             if id isNL xx5_5
                              then return ()
                              else throwError (strMsg "not match")
                             atp <- dv_afterPegM
                             return ()
                             return (id mkPegFile pr md empty pp p atp)]
p_pragma = foldl1 mplus [do dv_spacesM >> return ()
                            xx6_6 <- dvCharsM
                            if const True xx6_6
                             then return ()
                             else throwError (strMsg "not match")
                            case xx6_6 of
                                '{' -> return ()
                                _ -> throwError (strMsg "not match")
                            let '{' = xx6_6
                            return ()
                            xx7_7 <- dvCharsM
                            if const True xx7_7
                             then return ()
                             else throwError (strMsg "not match")
                            case xx7_7 of
                                '-' -> return ()
                                _ -> throwError (strMsg "not match")
                            let '-' = xx7_7
                            return ()
                            xx8_8 <- dvCharsM
                            if const True xx8_8
                             then return ()
                             else throwError (strMsg "not match")
                            case xx8_8 of
                                '#' -> return ()
                                _ -> throwError (strMsg "not match")
                            let '#' = xx8_8
                            return ()
                            s <- dv_pragmaStrM
                            return ()
                            dv_pragmaEndM >> return ()
                            dv_spacesM >> return ()
                            return (id just s),
                         do dv_spacesM >> return ()
                            return (id nothing)]
p_pragmaStr = foldl1 mplus [do d_9 <- get
                               flipMaybe (do dv_pragmaEndM >> return ())
                               put d_9
                               xx9_10 <- dvCharsM
                               let c = xx9_10
                               if const True xx9_10
                                then return ()
                                else throwError (strMsg "not match")
                               s <- dv_pragmaStrM
                               return ()
                               return (id cons c s),
                            do return (id empty)]
p_pragmaEnd = foldl1 mplus [do xx10_11 <- dvCharsM
                               if const True xx10_11
                                then return ()
                                else throwError (strMsg "not match")
                               case xx10_11 of
                                   '#' -> return ()
                                   _ -> throwError (strMsg "not match")
                               let '#' = xx10_11
                               return ()
                               xx11_12 <- dvCharsM
                               if const True xx11_12
                                then return ()
                                else throwError (strMsg "not match")
                               case xx11_12 of
                                   '-' -> return ()
                                   _ -> throwError (strMsg "not match")
                               let '-' = xx11_12
                               return ()
                               xx12_13 <- dvCharsM
                               if const True xx12_13
                                then return ()
                                else throwError (strMsg "not match")
                               case xx12_13 of
                                   '}' -> return ()
                                   _ -> throwError (strMsg "not match")
                               let '}' = xx12_13
                               return ()
                               return (id nil)]
p_moduleDec = foldl1 mplus [do xx13_14 <- dvCharsM
                               if const True xx13_14
                                then return ()
                                else throwError (strMsg "not match")
                               case xx13_14 of
                                   'm' -> return ()
                                   _ -> throwError (strMsg "not match")
                               let 'm' = xx13_14
                               return ()
                               xx14_15 <- dvCharsM
                               if const True xx14_15
                                then return ()
                                else throwError (strMsg "not match")
                               case xx14_15 of
                                   'o' -> return ()
                                   _ -> throwError (strMsg "not match")
                               let 'o' = xx14_15
                               return ()
                               xx15_16 <- dvCharsM
                               if const True xx15_16
                                then return ()
                                else throwError (strMsg "not match")
                               case xx15_16 of
                                   'd' -> return ()
                                   _ -> throwError (strMsg "not match")
                               let 'd' = xx15_16
                               return ()
                               xx16_17 <- dvCharsM
                               if const True xx16_17
                                then return ()
                                else throwError (strMsg "not match")
                               case xx16_17 of
                                   'u' -> return ()
                                   _ -> throwError (strMsg "not match")
                               let 'u' = xx16_17
                               return ()
                               xx17_18 <- dvCharsM
                               if const True xx17_18
                                then return ()
                                else throwError (strMsg "not match")
                               case xx17_18 of
                                   'l' -> return ()
                                   _ -> throwError (strMsg "not match")
                               let 'l' = xx17_18
                               return ()
                               xx18_19 <- dvCharsM
                               if const True xx18_19
                                then return ()
                                else throwError (strMsg "not match")
                               case xx18_19 of
                                   'e' -> return ()
                                   _ -> throwError (strMsg "not match")
                               let 'e' = xx18_19
                               return ()
                               s <- dv_moduleDecStrM
                               return ()
                               dv_whrM >> return ()
                               return (id just s),
                            do return (id nothing)]
p_moduleDecStr = foldl1 mplus [do d_20 <- get
                                  flipMaybe (do dv_whrM >> return ())
                                  put d_20
                                  xx19_21 <- dvCharsM
                                  let c = xx19_21
                                  if const True xx19_21
                                   then return ()
                                   else throwError (strMsg "not match")
                                  s <- dv_moduleDecStrM
                                  return ()
                                  return (id cons c s),
                               do return (id empty)]
p_whr = foldl1 mplus [do xx20_22 <- dvCharsM
                         if const True xx20_22
                          then return ()
                          else throwError (strMsg "not match")
                         case xx20_22 of
                             'w' -> return ()
                             _ -> throwError (strMsg "not match")
                         let 'w' = xx20_22
                         return ()
                         xx21_23 <- dvCharsM
                         if const True xx21_23
                          then return ()
                          else throwError (strMsg "not match")
                         case xx21_23 of
                             'h' -> return ()
                             _ -> throwError (strMsg "not match")
                         let 'h' = xx21_23
                         return ()
                         xx22_24 <- dvCharsM
                         if const True xx22_24
                          then return ()
                          else throwError (strMsg "not match")
                         case xx22_24 of
                             'e' -> return ()
                             _ -> throwError (strMsg "not match")
                         let 'e' = xx22_24
                         return ()
                         xx23_25 <- dvCharsM
                         if const True xx23_25
                          then return ()
                          else throwError (strMsg "not match")
                         case xx23_25 of
                             'r' -> return ()
                             _ -> throwError (strMsg "not match")
                         let 'r' = xx23_25
                         return ()
                         xx24_26 <- dvCharsM
                         if const True xx24_26
                          then return ()
                          else throwError (strMsg "not match")
                         case xx24_26 of
                             'e' -> return ()
                             _ -> throwError (strMsg "not match")
                         let 'e' = xx24_26
                         return ()
                         return (id nil)]
p_preImpPap = foldl1 mplus [do d_27 <- get
                               flipMaybe (do dv_importPapillonM >> return ())
                               put d_27
                               d_28 <- get
                               flipMaybe (do dv_papM >> return ())
                               put d_28
                               xx25_29 <- dvCharsM
                               let c = xx25_29
                               if id const true xx25_29
                                then return ()
                                else throwError (strMsg "not match")
                               pip <- dv_preImpPapM
                               return ()
                               return (id cons c pip),
                            do return (id empty)]
p_prePeg = foldl1 mplus [do d_30 <- get
                            flipMaybe (do dv_papM >> return ())
                            put d_30
                            xx26_31 <- dvCharsM
                            let c = xx26_31
                            if id const true xx26_31
                             then return ()
                             else throwError (strMsg "not match")
                            pp <- dv_prePegM
                            return ()
                            return (id cons c pp),
                         do return (id empty)]
p_afterPeg = foldl1 mplus [do xx27_32 <- dvCharsM
                              let c = xx27_32
                              if id const true xx27_32
                               then return ()
                               else throwError (strMsg "not match")
                              atp <- dv_afterPegM
                              return ()
                              return (id cons c atp),
                           do return (id empty)]
p_importPapillon = foldl1 mplus [do xx28_33 <- dv_varTokenM
                                    case xx28_33 of
                                        "import" -> return ()
                                        _ -> throwError (strMsg "not match")
                                    let "import" = xx28_33
                                    return ()
                                    xx29_34 <- dv_typTokenM
                                    case xx29_34 of
                                        "Text" -> return ()
                                        _ -> throwError (strMsg "not match")
                                    let "Text" = xx29_34
                                    return ()
                                    xx30_35 <- dvCharsM
                                    if const True xx30_35
                                     then return ()
                                     else throwError (strMsg "not match")
                                    case xx30_35 of
                                        '.' -> return ()
                                        _ -> throwError (strMsg "not match")
                                    let '.' = xx30_35
                                    return ()
                                    dv_spacesM >> return ()
                                    xx31_36 <- dv_typTokenM
                                    case xx31_36 of
                                        "Papillon" -> return ()
                                        _ -> throwError (strMsg "not match")
                                    let "Papillon" = xx31_36
                                    return ()
                                    return (id nil)]
p_varToken = foldl1 mplus [do v <- dv_variableM
                              return ()
                              dv_spacesM >> return ()
                              return (id v)]
p_typToken = foldl1 mplus [do t <- dv_typM
                              return ()
                              dv_spacesM >> return ()
                              return (id t)]
p_pap = foldl1 mplus [do xx32_37 <- dvCharsM
                         if id isNL xx32_37
                          then return ()
                          else throwError (strMsg "not match")
                         xx33_38 <- dvCharsM
                         if id isOpenBr xx33_38
                          then return ()
                          else throwError (strMsg "not match")
                         xx34_39 <- dvCharsM
                         if id isP xx34_39
                          then return ()
                          else throwError (strMsg "not match")
                         xx35_40 <- dvCharsM
                         if id isA xx35_40
                          then return ()
                          else throwError (strMsg "not match")
                         xx36_41 <- dvCharsM
                         if id isP xx36_41
                          then return ()
                          else throwError (strMsg "not match")
                         xx37_42 <- dvCharsM
                         if id isI xx37_42
                          then return ()
                          else throwError (strMsg "not match")
                         xx38_43 <- dvCharsM
                         if id isL xx38_43
                          then return ()
                          else throwError (strMsg "not match")
                         xx39_44 <- dvCharsM
                         if id isL xx39_44
                          then return ()
                          else throwError (strMsg "not match")
                         xx40_45 <- dvCharsM
                         if id isO xx40_45
                          then return ()
                          else throwError (strMsg "not match")
                         xx41_46 <- dvCharsM
                         if id isN xx41_46
                          then return ()
                          else throwError (strMsg "not match")
                         xx42_47 <- dvCharsM
                         if id isBar xx42_47
                          then return ()
                          else throwError (strMsg "not match")
                         xx43_48 <- dvCharsM
                         if id isNL xx43_48
                          then return ()
                          else throwError (strMsg "not match")
                         return (id nil)]
p_peg = foldl1 mplus [do dv_spacesM >> return ()
                         s <- dv_sourceTypeM
                         return ()
                         p <- dv_peg_M
                         return ()
                         return (id mkTTPeg s p),
                      do p <- dv_peg_M
                         return ()
                         return (id mkTTPeg tString p)]
p_sourceType = foldl1 mplus [do xx44_49 <- dv_varTokenM
                                case xx44_49 of
                                    "source" -> return ()
                                    _ -> throwError (strMsg "not match")
                                let "source" = xx44_49
                                return ()
                                xx45_50 <- dvCharsM
                                if const True xx45_50
                                 then return ()
                                 else throwError (strMsg "not match")
                                case xx45_50 of
                                    ':' -> return ()
                                    _ -> throwError (strMsg "not match")
                                let ':' = xx45_50
                                return ()
                                dv_spacesM >> return ()
                                v <- dv_typTokenM
                                return ()
                                return (id v)]
p_peg_ = foldl1 mplus [do dv_spacesM >> return ()
                          d <- dv_definitionM
                          return ()
                          p <- dv_peg_M
                          return ()
                          return (id cons d p),
                       do return (id empty)]
p_definition = foldl1 mplus [do v <- dv_variableM
                                return ()
                                dv_spacesM >> return ()
                                xx46_51 <- dvCharsM
                                if id isColon xx46_51
                                 then return ()
                                 else throwError (strMsg "not match")
                                xx47_52 <- dvCharsM
                                if id isColon xx47_52
                                 then return ()
                                 else throwError (strMsg "not match")
                                dv_spacesM >> return ()
                                t <- dv_typM
                                return ()
                                dv_spacesM >> return ()
                                xx48_53 <- dvCharsM
                                if id isEqual xx48_53
                                 then return ()
                                 else throwError (strMsg "not match")
                                dv_spacesM >> return ()
                                sel <- dv_selectionM
                                return ()
                                dv_spacesM >> return ()
                                xx49_54 <- dvCharsM
                                if id isSemi xx49_54
                                 then return ()
                                 else throwError (strMsg "not match")
                                return (id mkDef v t sel)]
p_selection = foldl1 mplus [do ex <- dv_expressionHsM
                               return ()
                               dv_spacesM >> return ()
                               xx50_55 <- dvCharsM
                               if id isSlash xx50_55
                                then return ()
                                else throwError (strMsg "not match")
                               dv_spacesM >> return ()
                               sel <- dv_selectionM
                               return ()
                               return (id cons ex sel),
                            do ex <- dv_expressionHsM
                               return ()
                               return (id cons ex empty)]
p_expressionHs = foldl1 mplus [do e <- dv_expressionM
                                  return ()
                                  dv_spacesM >> return ()
                                  xx51_56 <- dvCharsM
                                  if id isOpenWave xx51_56
                                   then return ()
                                   else throwError (strMsg "not match")
                                  dv_spacesM >> return ()
                                  h <- dv_hsExpM
                                  return ()
                                  dv_spacesM >> return ()
                                  xx52_57 <- dvCharsM
                                  if id isCloseWave xx52_57
                                   then return ()
                                   else throwError (strMsg "not match")
                                  return (id mkExpressionHs e h)]
p_expression = foldl1 mplus [do l <- dv_nameLeaf_M
                                return ()
                                dv_spacesM >> return ()
                                e <- dv_expressionM
                                return ()
                                return (id cons l e),
                             do return (id empty)]
p_nameLeaf_ = foldl1 mplus [do xx53_58 <- dvCharsM
                               if id isNot xx53_58
                                then return ()
                                else throwError (strMsg "not match")
                               nl <- dv_nameLeafM
                               return ()
                               return (id notAfter nl),
                            do nl <- dv_nameLeafM
                               return ()
                               return (id here nl)]
p_nameLeaf = foldl1 mplus [do n <- dv_patM
                              return ()
                              xx54_59 <- dvCharsM
                              if id isColon xx54_59
                               then return ()
                               else throwError (strMsg "not match")
                              l <- dv_leafM
                              return ()
                              return (id mkNameLeaf n l),
                           do n <- dv_patM
                              return ()
                              return (id mkNameLeaf n ctLeaf)]
p_pat = foldl1 mplus [do xx55_60 <- dv_variableM
                         case xx55_60 of
                             "_" -> return ()
                             _ -> throwError (strMsg "not match")
                         let "_" = xx55_60
                         return ()
                         return (id wildP),
                      do n <- dv_variableM
                         return ()
                         return (id strToPatQ n),
                      do t <- dv_typM
                         return ()
                         dv_spacesM >> return ()
                         ps <- dv_patsM
                         return ()
                         return (id conToPatQ t ps),
                      do xx56_61 <- dvCharsM
                         if id isChon xx56_61
                          then return ()
                          else throwError (strMsg "not match")
                         c <- dv_charLitM
                         return ()
                         xx57_62 <- dvCharsM
                         if id isChon xx57_62
                          then return ()
                          else throwError (strMsg "not match")
                         return (id charP c),
                      do xx58_63 <- dvCharsM
                         if id isDQ xx58_63
                          then return ()
                          else throwError (strMsg "not match")
                         s <- dv_stringLitM
                         return ()
                         xx59_64 <- dvCharsM
                         if id isDQ xx59_64
                          then return ()
                          else throwError (strMsg "not match")
                         return (id stringP s)]
p_charLit = foldl1 mplus [do xx60_65 <- dvCharsM
                             let c = xx60_65
                             if id isAlphaNumOt xx60_65
                              then return ()
                              else throwError (strMsg "not match")
                             return (id c),
                          do xx61_66 <- dvCharsM
                             if id isBS xx61_66
                              then return ()
                              else throwError (strMsg "not match")
                             xx62_67 <- dvCharsM
                             let c = xx62_67
                             if id elemNTs xx62_67
                              then return ()
                              else throwError (strMsg "not match")
                             return (id getNTs c)]
p_stringLit = foldl1 mplus [do d_68 <- get
                               flipMaybe (do dv_dqM >> return ())
                               put d_68
                               xx63_69 <- dvCharsM
                               let c = xx63_69
                               if const True xx63_69
                                then return ()
                                else throwError (strMsg "not match")
                               s <- dv_stringLitM
                               return ()
                               return (id cons c s),
                            do return (id empty)]
p_dq = foldl1 mplus [do xx64_70 <- dvCharsM
                        if const True xx64_70
                         then return ()
                         else throwError (strMsg "not match")
                        case xx64_70 of
                            '"' -> return ()
                            _ -> throwError (strMsg "not match")
                        let '"' = xx64_70
                        return ()
                        return (id nil)]
p_pats = foldl1 mplus [do p <- dv_patM
                          return ()
                          ps <- dv_patsM
                          return ()
                          return (id cons p ps),
                       do return (id empty)]
p_leaf = foldl1 mplus [do t <- dv_testM
                          return ()
                          return (id left t),
                       do v <- dv_variableM
                          return ()
                          return (id right v)]
p_test = foldl1 mplus [do xx65_71 <- dvCharsM
                          if id isOpenBr xx65_71
                           then return ()
                           else throwError (strMsg "not match")
                          h <- dv_hsExpM
                          return ()
                          xx66_72 <- dvCharsM
                          if id isCloseBr xx66_72
                           then return ()
                           else throwError (strMsg "not match")
                          return (id getEx h)]
p_hsExp = foldl1 mplus [do v <- dv_variableM
                           return ()
                           dv_spacesM >> return ()
                           h <- dv_hsExpM
                           return ()
                           return (id apply v h),
                        do v <- dv_variableM
                           return ()
                           return (id toExp v)]
p_typ = foldl1 mplus [do u <- dv_upperM
                         return ()
                         t <- dv_tvtailM
                         return ()
                         return (id cons u t)]
p_variable = foldl1 mplus [do l <- dv_lowerM
                              return ()
                              t <- dv_tvtailM
                              return ()
                              return (id cons l t)]
p_tvtail = foldl1 mplus [do a <- dv_alphaM
                            return ()
                            t <- dv_tvtailM
                            return ()
                            return (id cons a t),
                         do return (id empty)]
p_alpha = foldl1 mplus [do u <- dv_upperM
                           return ()
                           return (id u),
                        do l <- dv_lowerM
                           return ()
                           return (id l),
                        do d <- dv_digitM
                           return ()
                           return (id d)]
p_upper = foldl1 mplus [do xx67_73 <- dvCharsM
                           let u = xx67_73
                           if id isUpper xx67_73
                            then return ()
                            else throwError (strMsg "not match")
                           return (id u)]
p_lower = foldl1 mplus [do xx68_74 <- dvCharsM
                           let l = xx68_74
                           if id isLowerU xx68_74
                            then return ()
                            else throwError (strMsg "not match")
                           return (id l)]
p_digit = foldl1 mplus [do xx69_75 <- dvCharsM
                           let d = xx69_75
                           if id isDigit xx69_75
                            then return ()
                            else throwError (strMsg "not match")
                           return (id d)]
p_spaces = foldl1 mplus [do dv_spaceM >> return ()
                            dv_spacesM >> return ()
                            return (id nil),
                         do return (id nil)]
p_space = foldl1 mplus [do xx70_76 <- dvCharsM
                           if id isSpace xx70_76
                            then return ()
                            else throwError (strMsg "not match")
                           return (id nil),
                        do xx71_77 <- dvCharsM
                           if const True xx71_77
                            then return ()
                            else throwError (strMsg "not match")
                           case xx71_77 of
                               '-' -> return ()
                               _ -> throwError (strMsg "not match")
                           let '-' = xx71_77
                           return ()
                           xx72_78 <- dvCharsM
                           if const True xx72_78
                            then return ()
                            else throwError (strMsg "not match")
                           case xx72_78 of
                               '-' -> return ()
                               _ -> throwError (strMsg "not match")
                           let '-' = xx72_78
                           return ()
                           dv_notNLStringM >> return ()
                           dv_nlM >> return ()
                           return (id nil),
                        do dv_commentM >> return ()
                           return (id nil)]
p_notNLString = foldl1 mplus [do d_79 <- get
                                 flipMaybe (do dv_nlM >> return ())
                                 put d_79
                                 xx73_80 <- dvCharsM
                                 let c = xx73_80
                                 if const True xx73_80
                                  then return ()
                                  else throwError (strMsg "not match")
                                 s <- dv_notNLStringM
                                 return ()
                                 return (id cons c s),
                              do return (id empty)]
p_nl = foldl1 mplus [do xx74_81 <- dvCharsM
                        if id isNL xx74_81
                         then return ()
                         else throwError (strMsg "not match")
                        return (id nil)]
p_comment = foldl1 mplus [do xx75_82 <- dvCharsM
                             if const True xx75_82
                              then return ()
                              else throwError (strMsg "not match")
                             case xx75_82 of
                                 '{' -> return ()
                                 _ -> throwError (strMsg "not match")
                             let '{' = xx75_82
                             return ()
                             xx76_83 <- dvCharsM
                             if const True xx76_83
                              then return ()
                              else throwError (strMsg "not match")
                             case xx76_83 of
                                 '-' -> return ()
                                 _ -> throwError (strMsg "not match")
                             let '-' = xx76_83
                             return ()
                             d_84 <- get
                             flipMaybe (do xx77_85 <- dvCharsM
                                           if const True xx77_85
                                            then return ()
                                            else throwError (strMsg "not match")
                                           case xx77_85 of
                                               '#' -> return ()
                                               _ -> throwError (strMsg "not match")
                                           let '#' = xx77_85
                                           return ())
                             put d_84
                             dv_commentsM >> return ()
                             dv_comEndM >> return ()
                             return (id nil)]
p_comments = foldl1 mplus [do dv_notComStrM >> return ()
                              dv_commentM >> return ()
                              dv_commentsM >> return ()
                              return (id nil),
                           do dv_notComStrM >> return ()
                              return (id nil)]
p_notComStr = foldl1 mplus [do d_86 <- get
                               flipMaybe (do dv_commentM >> return ())
                               put d_86
                               d_87 <- get
                               flipMaybe (do dv_comEndM >> return ())
                               put d_87
                               xx78_88 <- dvCharsM
                               if const True xx78_88
                                then return ()
                                else throwError (strMsg "not match")
                               dv_notComStrM >> return ()
                               return (id nil),
                            do return (id nil)]
p_comEnd = foldl1 mplus [do xx79_89 <- dvCharsM
                            if const True xx79_89
                             then return ()
                             else throwError (strMsg "not match")
                            case xx79_89 of
                                '-' -> return ()
                                _ -> throwError (strMsg "not match")
                            let '-' = xx79_89
                            return ()
                            xx80_90 <- dvCharsM
                            if const True xx80_90
                             then return ()
                             else throwError (strMsg "not match")
                            case xx80_90 of
                                '}' -> return ()
                                _ -> throwError (strMsg "not match")
                            let '}' = xx80_90
                            return ()
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