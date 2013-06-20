{-# LANGUAGE FlexibleContexts, TemplateHaskell , FlexibleContexts, PackageImports, TypeFamilies #-}
module  Text.Papillon.Parser (
	Peg,
	Definition,
	ExpressionHs,
	NameLeaf,
	parse,
	dv_peg,
	dv_pegFile,
	Leaf_(..)
)  where
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Monad.Trans.Error (Error (..))



import Data.Char
import Language.Haskell.TH

type MaybeString = Maybe String

type Nil = ()
type Leaf = Either String ExR
data Leaf_ = NotAfter Leaf | Here Leaf
notAfter, here :: Leaf -> Leaf_
notAfter = NotAfter
here = Here
type NameLeaf = (PatQ, Leaf_)
type Expression = [NameLeaf]
type ExpressionHs = (Expression, ExR)
type Selection = [ExpressionHs]
type Typ = Name
type Definition = (String, Typ, Selection)
type Peg = [Definition]
type TTPeg = (TypeQ, TypeQ, Peg)

type Ex = ExpQ -> ExpQ
type ExR = ExpQ

ctLeaf :: Leaf_
ctLeaf = Here $ Right $ varE (mkName "const") `appE` conE (mkName "True")

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

isEqual, isSlash, isSemi, isColon, isOpenWave, isCloseWave, isLowerU, isNot,
	isChon, isDQ :: Char -> Bool
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

isOpenBr, isP, isA, isI, isL, isO, isN, isBar, isCloseBr, isNL :: Char -> Bool
[isOpenBr, isP, isA, isI, isL, isO, isN, isBar, isCloseBr, isNL] =
	map (==) "[pailon|]\n"

{-
tString, tChar :: TypeQ
tString = varT ''String
tChar = varT ''Char
-}

tString, tChar :: String
tString = "String"
tChar = "Char"
mkTTPeg :: String -> String -> Peg -> TTPeg
mkTTPeg s t p = (conT $ mkName s, conT $ mkName t, p)

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
              dv_tokenType :: (Result String),
              dv_peg_ :: (Result Peg),
              dv_definition :: (Result Definition),
              dv_selection :: (Result Selection),
              dv_expressionHs :: (Result ExpressionHs),
              dv_expression :: (Result Expression),
              dv_nameLeaf :: (Result NameLeaf),
              dv_pat :: (Result PatQ),
              dv_stringLit :: (Result String),
              dv_dq :: (Result Nil),
              dv_pats :: (Result PatQs),
              dv_leaf_ :: (Result Leaf_),
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
              dvChars :: (Result Char)}
parse :: String -> Derivs
parse s = d
          where d = Derivs pegFile pragma pragmaStr pragmaEnd moduleDec moduleDecStr whr preImpPap prePeg afterPeg importPapillon varToken typToken pap peg sourceType tokenType peg_ definition selection expressionHs expression nameLeaf pat stringLit dq pats leaf_ leaf test hsExp typ variable tvtail alpha upper lower digit spaces space notNLString nl char
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
                tokenType = runStateT p_tokenType d
                peg_ = runStateT p_peg_ d
                definition = runStateT p_definition d
                selection = runStateT p_selection d
                expressionHs = runStateT p_expressionHs d
                expression = runStateT p_expression d
                nameLeaf = runStateT p_nameLeaf d
                pat = runStateT p_pat d
                stringLit = runStateT p_stringLit d
                dq = runStateT p_dq d
                pats = runStateT p_pats d
                leaf_ = runStateT p_leaf_ d
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
dv_tokenTypeM :: PackratM String
dv_peg_M :: PackratM Peg
dv_definitionM :: PackratM Definition
dv_selectionM :: PackratM Selection
dv_expressionHsM :: PackratM ExpressionHs
dv_expressionM :: PackratM Expression
dv_nameLeafM :: PackratM NameLeaf
dv_patM :: PackratM PatQ
dv_stringLitM :: PackratM String
dv_dqM :: PackratM Nil
dv_patsM :: PackratM PatQs
dv_leaf_M :: PackratM Leaf_
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
dv_tokenTypeM = StateT dv_tokenType
dv_peg_M = StateT dv_peg_
dv_definitionM = StateT dv_definition
dv_selectionM = StateT dv_selection
dv_expressionHsM = StateT dv_expressionHs
dv_expressionM = StateT dv_expression
dv_nameLeafM = StateT dv_nameLeaf
dv_patM = StateT dv_pat
dv_stringLitM = StateT dv_stringLit
dv_dqM = StateT dv_dq
dv_patsM = StateT dv_pats
dv_leaf_M = StateT dv_leaf_
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
dvCharsM :: PackratM Char
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
p_tokenType :: PackratM String
p_peg_ :: PackratM Peg
p_definition :: PackratM Definition
p_selection :: PackratM Selection
p_expressionHs :: PackratM ExpressionHs
p_expression :: PackratM Expression
p_nameLeaf :: PackratM NameLeaf
p_pat :: PackratM PatQ
p_stringLit :: PackratM String
p_dq :: PackratM Nil
p_pats :: PackratM PatQs
p_leaf_ :: PackratM Leaf_
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
p_pegFile = msum [do pr <- dv_pragmaM
                     md <- dv_moduleDecM
                     pip <- dv_preImpPapM
                     _ <- dv_importPapillonM
                     pp <- dv_prePegM
                     _ <- dv_papM
                     p <- dv_pegM
                     _ <- dv_spacesM
                     xx0_0 <- dvCharsM
                     if id isBar xx0_0
                      then return ()
                      else throwError (strMsg "not match")
                     case xx0_0 of
                         _ -> return ()
                     let _ = xx0_0
                     return ()
                     xx1_1 <- dvCharsM
                     if id isCloseBr xx1_1
                      then return ()
                      else throwError (strMsg "not match")
                     case xx1_1 of
                         _ -> return ()
                     let _ = xx1_1
                     return ()
                     xx2_2 <- dvCharsM
                     if id isNL xx2_2
                      then return ()
                      else throwError (strMsg "not match")
                     case xx2_2 of
                         _ -> return ()
                     let _ = xx2_2
                     return ()
                     atp <- dv_afterPegM
                     return (id mkPegFile pr md pip pp p atp),
                  do pr <- dv_pragmaM
                     md <- dv_moduleDecM
                     pp <- dv_prePegM
                     _ <- dv_papM
                     p <- dv_pegM
                     _ <- dv_spacesM
                     xx3_3 <- dvCharsM
                     if id isBar xx3_3
                      then return ()
                      else throwError (strMsg "not match")
                     case xx3_3 of
                         _ -> return ()
                     let _ = xx3_3
                     return ()
                     xx4_4 <- dvCharsM
                     if id isCloseBr xx4_4
                      then return ()
                      else throwError (strMsg "not match")
                     case xx4_4 of
                         _ -> return ()
                     let _ = xx4_4
                     return ()
                     xx5_5 <- dvCharsM
                     if id isNL xx5_5
                      then return ()
                      else throwError (strMsg "not match")
                     case xx5_5 of
                         _ -> return ()
                     let _ = xx5_5
                     return ()
                     atp <- dv_afterPegM
                     return (id mkPegFile pr md empty pp p atp)]
p_pragma = msum [do _ <- dv_spacesM
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
                    _ <- dv_pragmaEndM
                    _ <- dv_spacesM
                    return (id just s),
                 do _ <- dv_spacesM
                    return (id nothing)]
p_pragmaStr = msum [do d_9 <- get
                       flipMaybe dv_pragmaEndM
                       put d_9
                       xx9_10 <- dvCharsM
                       if const True xx9_10
                        then return ()
                        else throwError (strMsg "not match")
                       case xx9_10 of
                           _ -> return ()
                       let c = xx9_10
                       return ()
                       s <- dv_pragmaStrM
                       return (id cons c s),
                    do return (id empty)]
p_pragmaEnd = msum [do xx10_11 <- dvCharsM
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
p_moduleDec = msum [do xx13_14 <- dvCharsM
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
                       _ <- dv_whrM
                       return (id just s),
                    do return (id nothing)]
p_moduleDecStr = msum [do d_20 <- get
                          flipMaybe dv_whrM
                          put d_20
                          xx19_21 <- dvCharsM
                          if const True xx19_21
                           then return ()
                           else throwError (strMsg "not match")
                          case xx19_21 of
                              _ -> return ()
                          let c = xx19_21
                          return ()
                          s <- dv_moduleDecStrM
                          return (id cons c s),
                       do return (id empty)]
p_whr = msum [do xx20_22 <- dvCharsM
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
p_preImpPap = msum [do d_27 <- get
                       flipMaybe dv_importPapillonM
                       put d_27
                       d_28 <- get
                       flipMaybe dv_papM
                       put d_28
                       xx25_29 <- dvCharsM
                       if id const true xx25_29
                        then return ()
                        else throwError (strMsg "not match")
                       case xx25_29 of
                           _ -> return ()
                       let c = xx25_29
                       return ()
                       pip <- dv_preImpPapM
                       return (id cons c pip),
                    do return (id empty)]
p_prePeg = msum [do d_30 <- get
                    flipMaybe dv_papM
                    put d_30
                    xx26_31 <- dvCharsM
                    if id const true xx26_31
                     then return ()
                     else throwError (strMsg "not match")
                    case xx26_31 of
                        _ -> return ()
                    let c = xx26_31
                    return ()
                    pp <- dv_prePegM
                    return (id cons c pp),
                 do return (id empty)]
p_afterPeg = msum [do xx27_32 <- dvCharsM
                      if id const true xx27_32
                       then return ()
                       else throwError (strMsg "not match")
                      case xx27_32 of
                          _ -> return ()
                      let c = xx27_32
                      return ()
                      atp <- dv_afterPegM
                      return (id cons c atp),
                   do return (id empty)]
p_importPapillon = msum [do xx28_33 <- dv_varTokenM
                            case xx28_33 of
                                "import" -> return ()
                                _ -> throwError (strMsg "not match")
                            "import" <- return xx28_33
                            xx29_34 <- dv_typTokenM
                            case xx29_34 of
                                "Text" -> return ()
                                _ -> throwError (strMsg "not match")
                            "Text" <- return xx29_34
                            xx30_35 <- dvCharsM
                            if const True xx30_35
                             then return ()
                             else throwError (strMsg "not match")
                            case xx30_35 of
                                '.' -> return ()
                                _ -> throwError (strMsg "not match")
                            let '.' = xx30_35
                            return ()
                            _ <- dv_spacesM
                            xx31_36 <- dv_typTokenM
                            case xx31_36 of
                                "Papillon" -> return ()
                                _ -> throwError (strMsg "not match")
                            "Papillon" <- return xx31_36
                            return (id nil)]
p_varToken = msum [do v <- dv_variableM
                      _ <- dv_spacesM
                      return (id v)]
p_typToken = msum [do t <- dv_typM
                      _ <- dv_spacesM
                      return (id t)]
p_pap = msum [do xx32_37 <- dvCharsM
                 if id isNL xx32_37
                  then return ()
                  else throwError (strMsg "not match")
                 case xx32_37 of
                     _ -> return ()
                 let _ = xx32_37
                 return ()
                 xx33_38 <- dvCharsM
                 if id isOpenBr xx33_38
                  then return ()
                  else throwError (strMsg "not match")
                 case xx33_38 of
                     _ -> return ()
                 let _ = xx33_38
                 return ()
                 xx34_39 <- dvCharsM
                 if id isP xx34_39
                  then return ()
                  else throwError (strMsg "not match")
                 case xx34_39 of
                     _ -> return ()
                 let _ = xx34_39
                 return ()
                 xx35_40 <- dvCharsM
                 if id isA xx35_40
                  then return ()
                  else throwError (strMsg "not match")
                 case xx35_40 of
                     _ -> return ()
                 let _ = xx35_40
                 return ()
                 xx36_41 <- dvCharsM
                 if id isP xx36_41
                  then return ()
                  else throwError (strMsg "not match")
                 case xx36_41 of
                     _ -> return ()
                 let _ = xx36_41
                 return ()
                 xx37_42 <- dvCharsM
                 if id isI xx37_42
                  then return ()
                  else throwError (strMsg "not match")
                 case xx37_42 of
                     _ -> return ()
                 let _ = xx37_42
                 return ()
                 xx38_43 <- dvCharsM
                 if id isL xx38_43
                  then return ()
                  else throwError (strMsg "not match")
                 case xx38_43 of
                     _ -> return ()
                 let _ = xx38_43
                 return ()
                 xx39_44 <- dvCharsM
                 if id isL xx39_44
                  then return ()
                  else throwError (strMsg "not match")
                 case xx39_44 of
                     _ -> return ()
                 let _ = xx39_44
                 return ()
                 xx40_45 <- dvCharsM
                 if id isO xx40_45
                  then return ()
                  else throwError (strMsg "not match")
                 case xx40_45 of
                     _ -> return ()
                 let _ = xx40_45
                 return ()
                 xx41_46 <- dvCharsM
                 if id isN xx41_46
                  then return ()
                  else throwError (strMsg "not match")
                 case xx41_46 of
                     _ -> return ()
                 let _ = xx41_46
                 return ()
                 xx42_47 <- dvCharsM
                 if id isBar xx42_47
                  then return ()
                  else throwError (strMsg "not match")
                 case xx42_47 of
                     _ -> return ()
                 let _ = xx42_47
                 return ()
                 xx43_48 <- dvCharsM
                 if id isNL xx43_48
                  then return ()
                  else throwError (strMsg "not match")
                 case xx43_48 of
                     _ -> return ()
                 let _ = xx43_48
                 return ()
                 return (id nil)]
p_peg = msum [do _ <- dv_spacesM
                 s <- dv_sourceTypeM
                 t <- dv_tokenTypeM
                 p <- dv_peg_M
                 return (id mkTTPeg s t p),
              do p <- dv_peg_M
                 return (id mkTTPeg tString tChar p)]
p_sourceType = msum [do xx44_49 <- dv_varTokenM
                        case xx44_49 of
                            "source" -> return ()
                            _ -> throwError (strMsg "not match")
                        "source" <- return xx44_49
                        xx45_50 <- dvCharsM
                        if const True xx45_50
                         then return ()
                         else throwError (strMsg "not match")
                        case xx45_50 of
                            ':' -> return ()
                            _ -> throwError (strMsg "not match")
                        let ':' = xx45_50
                        return ()
                        _ <- dv_spacesM
                        v <- dv_typTokenM
                        return (id v)]
p_tokenType = msum [do xx46_51 <- dv_varTokenM
                       case xx46_51 of
                           "token" -> return ()
                           _ -> throwError (strMsg "not match")
                       "token" <- return xx46_51
                       xx47_52 <- dvCharsM
                       if const True xx47_52
                        then return ()
                        else throwError (strMsg "not match")
                       case xx47_52 of
                           ':' -> return ()
                           _ -> throwError (strMsg "not match")
                       let ':' = xx47_52
                       return ()
                       _ <- dv_spacesM
                       v <- dv_typTokenM
                       return (id v)]
p_peg_ = msum [do _ <- dv_spacesM
                  d <- dv_definitionM
                  p <- dv_peg_M
                  return (id cons d p),
               do return (id empty)]
p_definition = msum [do v <- dv_variableM
                        _ <- dv_spacesM
                        xx48_53 <- dvCharsM
                        if id isColon xx48_53
                         then return ()
                         else throwError (strMsg "not match")
                        case xx48_53 of
                            _ -> return ()
                        let _ = xx48_53
                        return ()
                        xx49_54 <- dvCharsM
                        if id isColon xx49_54
                         then return ()
                         else throwError (strMsg "not match")
                        case xx49_54 of
                            _ -> return ()
                        let _ = xx49_54
                        return ()
                        _ <- dv_spacesM
                        t <- dv_typM
                        _ <- dv_spacesM
                        xx50_55 <- dvCharsM
                        if id isEqual xx50_55
                         then return ()
                         else throwError (strMsg "not match")
                        case xx50_55 of
                            _ -> return ()
                        let _ = xx50_55
                        return ()
                        _ <- dv_spacesM
                        sel <- dv_selectionM
                        _ <- dv_spacesM
                        xx51_56 <- dvCharsM
                        if id isSemi xx51_56
                         then return ()
                         else throwError (strMsg "not match")
                        case xx51_56 of
                            _ -> return ()
                        let _ = xx51_56
                        return ()
                        return (id mkDef v t sel)]
p_selection = msum [do ex <- dv_expressionHsM
                       _ <- dv_spacesM
                       xx52_57 <- dvCharsM
                       if id isSlash xx52_57
                        then return ()
                        else throwError (strMsg "not match")
                       case xx52_57 of
                           _ -> return ()
                       let _ = xx52_57
                       return ()
                       _ <- dv_spacesM
                       sel <- dv_selectionM
                       return (id cons ex sel),
                    do ex <- dv_expressionHsM
                       return (id cons ex empty)]
p_expressionHs = msum [do e <- dv_expressionM
                          _ <- dv_spacesM
                          xx53_58 <- dvCharsM
                          if id isOpenWave xx53_58
                           then return ()
                           else throwError (strMsg "not match")
                          case xx53_58 of
                              _ -> return ()
                          let _ = xx53_58
                          return ()
                          _ <- dv_spacesM
                          h <- dv_hsExpM
                          _ <- dv_spacesM
                          xx54_59 <- dvCharsM
                          if id isCloseWave xx54_59
                           then return ()
                           else throwError (strMsg "not match")
                          case xx54_59 of
                              _ -> return ()
                          let _ = xx54_59
                          return ()
                          return (id mkExpressionHs e h)]
p_expression = msum [do l <- dv_nameLeafM
                        _ <- dv_spacesM
                        e <- dv_expressionM
                        return (id cons l e),
                     do return (id empty)]
p_nameLeaf = msum [do n <- dv_patM
                      xx55_60 <- dvCharsM
                      if id isColon xx55_60
                       then return ()
                       else throwError (strMsg "not match")
                      case xx55_60 of
                          _ -> return ()
                      let _ = xx55_60
                      return ()
                      l <- dv_leaf_M
                      return (id mkNameLeaf n l),
                   do n <- dv_patM
                      return (id mkNameLeaf n ctLeaf)]
p_pat = msum [do xx56_61 <- dv_variableM
                 case xx56_61 of
                     "_" -> return ()
                     _ -> throwError (strMsg "not match")
                 "_" <- return xx56_61
                 return (id wildP),
              do n <- dv_variableM
                 return (id strToPatQ n),
              do t <- dv_typM
                 _ <- dv_spacesM
                 ps <- dv_patsM
                 return (id conToPatQ t ps),
              do xx57_62 <- dvCharsM
                 if id isChon xx57_62
                  then return ()
                  else throwError (strMsg "not match")
                 case xx57_62 of
                     _ -> return ()
                 let _ = xx57_62
                 return ()
                 xx58_63 <- dvCharsM
                 if id const true xx58_63
                  then return ()
                  else throwError (strMsg "not match")
                 case xx58_63 of
                     _ -> return ()
                 let c = xx58_63
                 return ()
                 xx59_64 <- dvCharsM
                 if id isChon xx59_64
                  then return ()
                  else throwError (strMsg "not match")
                 case xx59_64 of
                     _ -> return ()
                 let _ = xx59_64
                 return ()
                 return (id charP c),
              do xx60_65 <- dvCharsM
                 if id isDQ xx60_65
                  then return ()
                  else throwError (strMsg "not match")
                 case xx60_65 of
                     _ -> return ()
                 let _ = xx60_65
                 return ()
                 s <- dv_stringLitM
                 xx61_66 <- dvCharsM
                 if id isDQ xx61_66
                  then return ()
                  else throwError (strMsg "not match")
                 case xx61_66 of
                     _ -> return ()
                 let _ = xx61_66
                 return ()
                 return (id stringP s)]
p_stringLit = msum [do d_67 <- get
                       flipMaybe dv_dqM
                       put d_67
                       xx62_68 <- dvCharsM
                       if const True xx62_68
                        then return ()
                        else throwError (strMsg "not match")
                       case xx62_68 of
                           _ -> return ()
                       let c = xx62_68
                       return ()
                       s <- dv_stringLitM
                       return (id cons c s),
                    do return (id empty)]
p_dq = msum [do xx63_69 <- dvCharsM
                if const True xx63_69
                 then return ()
                 else throwError (strMsg "not match")
                case xx63_69 of
                    '"' -> return ()
                    _ -> throwError (strMsg "not match")
                let '"' = xx63_69
                return ()
                return (id nil)]
p_pats = msum [do p <- dv_patM
                  ps <- dv_patsM
                  return (id cons p ps),
               do return (id empty)]
p_leaf_ = msum [do xx64_70 <- dvCharsM
                   if id isNot xx64_70
                    then return ()
                    else throwError (strMsg "not match")
                   case xx64_70 of
                       _ -> return ()
                   let _ = xx64_70
                   return ()
                   l <- dv_leafM
                   return (id notAfter l),
                do l <- dv_leafM
                   return (id here l)]
p_leaf = msum [do t <- dv_testM
                  return (id left t),
               do v <- dv_variableM
                  return (id right v)]
p_test = msum [do xx65_71 <- dvCharsM
                  if id isOpenBr xx65_71
                   then return ()
                   else throwError (strMsg "not match")
                  case xx65_71 of
                      _ -> return ()
                  let _ = xx65_71
                  return ()
                  h <- dv_hsExpM
                  xx66_72 <- dvCharsM
                  if id isCloseBr xx66_72
                   then return ()
                   else throwError (strMsg "not match")
                  case xx66_72 of
                      _ -> return ()
                  let _ = xx66_72
                  return ()
                  return (id getEx h)]
p_hsExp = msum [do v <- dv_variableM
                   _ <- dv_spacesM
                   h <- dv_hsExpM
                   return (id apply v h),
                do v <- dv_variableM
                   return (id toExp v)]
p_typ = msum [do u <- dv_upperM
                 t <- dv_tvtailM
                 return (id cons u t)]
p_variable = msum [do l <- dv_lowerM
                      t <- dv_tvtailM
                      return (id cons l t)]
p_tvtail = msum [do a <- dv_alphaM
                    t <- dv_tvtailM
                    return (id cons a t),
                 do return (id empty)]
p_alpha = msum [do u <- dv_upperM
                   return (id u),
                do l <- dv_lowerM
                   return (id l),
                do d <- dv_digitM
                   return (id d)]
p_upper = msum [do xx67_73 <- dvCharsM
                   if id isUpper xx67_73
                    then return ()
                    else throwError (strMsg "not match")
                   case xx67_73 of
                       _ -> return ()
                   let u = xx67_73
                   return ()
                   return (id u)]
p_lower = msum [do xx68_74 <- dvCharsM
                   if id isLowerU xx68_74
                    then return ()
                    else throwError (strMsg "not match")
                   case xx68_74 of
                       _ -> return ()
                   let l = xx68_74
                   return ()
                   return (id l)]
p_digit = msum [do xx69_75 <- dvCharsM
                   if id isDigit xx69_75
                    then return ()
                    else throwError (strMsg "not match")
                   case xx69_75 of
                       _ -> return ()
                   let d = xx69_75
                   return ()
                   return (id d)]
p_spaces = msum [do _ <- dv_spaceM
                    _ <- dv_spacesM
                    return (id nil),
                 do return (id nil)]
p_space = msum [do xx70_76 <- dvCharsM
                   if id isSpace xx70_76
                    then return ()
                    else throwError (strMsg "not match")
                   case xx70_76 of
                       _ -> return ()
                   let _ = xx70_76
                   return ()
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
                   _ <- dv_notNLStringM
                   _ <- dv_nlM
                   return (id nil)]
p_notNLString = msum [do d_79 <- get
                         flipMaybe dv_nlM
                         put d_79
                         xx73_80 <- dvCharsM
                         if const True xx73_80
                          then return ()
                          else throwError (strMsg "not match")
                         case xx73_80 of
                             _ -> return ()
                         let c = xx73_80
                         return ()
                         s <- dv_notNLStringM
                         return (id cons c s),
                      do return (id empty)]
p_nl = msum [do xx74_81 <- dvCharsM
                if id isNL xx74_81
                 then return ()
                 else throwError (strMsg "not match")
                case xx74_81 of
                    _ -> return ()
                let _ = xx74_81
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