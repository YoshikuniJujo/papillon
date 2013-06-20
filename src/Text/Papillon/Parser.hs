
{-# LANGUAGE PackageImports, FlexibleContexts, TemplateHaskell #-}

module Text.Papillon.Parser (
	Peg,
	Definition,
	NameLeaf,
	parse,
	dv_peg,
	dv_pegFile,
	Leaf_(..)
) where

import Data.Char
import Control.Monad
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Monad.Trans.Error (Error(..))
import Language.Haskell.TH

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

type Ex = ExpQ -> ExpQ
type ExR = ExpQ

ctLeaf :: Leaf_
ctLeaf = Here $ Right $ varE (mkName "const") `appE` conE (mkName "True")

left :: b -> Either a b
right :: a -> Either a b
left = Right
right = Left

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

type PegFile = (String, Peg, String)
mkPegFile :: String -> String -> b -> c -> (String, b, c)
mkPegFile x y z w = (x ++ "\n" ++ y, z, w)

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

flipMaybe :: (Error (ErrorType me), MonadError me) =>
	StateT s me a -> StateT s me ()
flipMaybe action = do
	err <- (action >> return False) `catchError` const (return True)
	unless err $ throwError $ strMsg "not error"
type PackratM = StateT Derivs (Either String)
type Result v = Either String ((v, Derivs))
data Derivs
    = Derivs {dv_pegFile :: (Result PegFile),
              dv_preImpPap :: (Result String),
              dv_prePeg :: (Result String),
              dv_afterPeg :: (Result String),
              dv_importPapillon :: (Result Nil),
              dv_varToken :: (Result String),
              dv_typToken :: (Result String),
              dv_pap :: (Result Nil),
              dv_peg :: (Result Peg),
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
              dvChars :: (Result Char)}
parse :: String -> Derivs
parse s = d
          where d = Derivs pegFile preImpPap prePeg afterPeg importPapillon varToken typToken pap peg definition selection expressionHs expression nameLeaf pat stringLit dq pats leaf_ leaf test hsExp typ variable tvtail alpha upper lower digit spaces space char
                pegFile = runStateT p_pegFile d
                preImpPap = runStateT p_preImpPap d
                prePeg = runStateT p_prePeg d
                afterPeg = runStateT p_afterPeg d
                importPapillon = runStateT p_importPapillon d
                varToken = runStateT p_varToken d
                typToken = runStateT p_typToken d
                pap = runStateT p_pap d
                peg = runStateT p_peg d
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
                char = flip runStateT d (do when (null s) (throwError (strMsg "eof"))
                                            c : s' <- return s
                                            put (parse s')
                                            return c)
dv_pegFileM :: PackratM PegFile
dv_preImpPapM :: PackratM String
dv_prePegM :: PackratM String
dv_afterPegM :: PackratM String
dv_importPapillonM :: PackratM Nil
dv_varTokenM :: PackratM String
dv_typTokenM :: PackratM String
dv_papM :: PackratM Nil
dv_pegM :: PackratM Peg
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
dv_pegFileM = StateT dv_pegFile
dv_preImpPapM = StateT dv_preImpPap
dv_prePegM = StateT dv_prePeg
dv_afterPegM = StateT dv_afterPeg
dv_importPapillonM = StateT dv_importPapillon
dv_varTokenM = StateT dv_varToken
dv_typTokenM = StateT dv_typToken
dv_papM = StateT dv_pap
dv_pegM = StateT dv_peg
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
dvCharsM :: PackratM Char
dvCharsM = StateT dvChars
p_pegFile :: PackratM PegFile
p_preImpPap :: PackratM String
p_prePeg :: PackratM String
p_afterPeg :: PackratM String
p_importPapillon :: PackratM Nil
p_varToken :: PackratM String
p_typToken :: PackratM String
p_pap :: PackratM Nil
p_peg :: PackratM Peg
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
p_pegFile = msum [do pip <- dv_preImpPapM
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
                     return (id mkPegFile pip pp p atp),
                  do pp <- dv_prePegM
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
                     return (id mkPegFile empty pp p atp)]
p_preImpPap = msum [do d_6 <- get
                       flipMaybe dv_importPapillonM
                       put d_6
                       d_7 <- get
                       flipMaybe dv_papM
                       put d_7
                       xx6_8 <- dvCharsM
                       if id const true xx6_8
                        then return ()
                        else throwError (strMsg "not match")
                       case xx6_8 of
                           _ -> return ()
                       let c = xx6_8
                       return ()
                       pip <- dv_preImpPapM
                       return (id cons c pip),
                    do return (id empty)]
p_prePeg = msum [do d_9 <- get
                    flipMaybe dv_papM
                    put d_9
                    xx7_10 <- dvCharsM
                    if id const true xx7_10
                     then return ()
                     else throwError (strMsg "not match")
                    case xx7_10 of
                        _ -> return ()
                    let c = xx7_10
                    return ()
                    pp <- dv_prePegM
                    return (id cons c pp),
                 do return (id empty)]
p_afterPeg = msum [do xx8_11 <- dvCharsM
                      if id const true xx8_11
                       then return ()
                       else throwError (strMsg "not match")
                      case xx8_11 of
                          _ -> return ()
                      let c = xx8_11
                      return ()
                      atp <- dv_afterPegM
                      return (id cons c atp),
                   do return (id empty)]
p_importPapillon = msum [do xx9_12 <- dv_varTokenM
                            case xx9_12 of
                                "import" -> return ()
                                _ -> throwError (strMsg "not match")
                            "import" <- return xx9_12
                            xx10_13 <- dv_typTokenM
                            case xx10_13 of
                                "Text" -> return ()
                                _ -> throwError (strMsg "not match")
                            "Text" <- return xx10_13
                            xx11_14 <- dvCharsM
                            if const True xx11_14
                             then return ()
                             else throwError (strMsg "not match")
                            case xx11_14 of
                                '.' -> return ()
                                _ -> throwError (strMsg "not match")
                            let '.' = xx11_14
                            return ()
                            _ <- dv_spacesM
                            xx12_15 <- dv_typTokenM
                            case xx12_15 of
                                "Papillon" -> return ()
                                _ -> throwError (strMsg "not match")
                            "Papillon" <- return xx12_15
                            return (id nil)]
p_varToken = msum [do v <- dv_variableM
                      _ <- dv_spacesM
                      return (id v)]
p_typToken = msum [do t <- dv_typM
                      _ <- dv_spacesM
                      return (id t)]
p_pap = msum [do xx13_16 <- dvCharsM
                 if id isNL xx13_16
                  then return ()
                  else throwError (strMsg "not match")
                 case xx13_16 of
                     _ -> return ()
                 let _ = xx13_16
                 return ()
                 xx14_17 <- dvCharsM
                 if id isOpenBr xx14_17
                  then return ()
                  else throwError (strMsg "not match")
                 case xx14_17 of
                     _ -> return ()
                 let _ = xx14_17
                 return ()
                 xx15_18 <- dvCharsM
                 if id isP xx15_18
                  then return ()
                  else throwError (strMsg "not match")
                 case xx15_18 of
                     _ -> return ()
                 let _ = xx15_18
                 return ()
                 xx16_19 <- dvCharsM
                 if id isA xx16_19
                  then return ()
                  else throwError (strMsg "not match")
                 case xx16_19 of
                     _ -> return ()
                 let _ = xx16_19
                 return ()
                 xx17_20 <- dvCharsM
                 if id isP xx17_20
                  then return ()
                  else throwError (strMsg "not match")
                 case xx17_20 of
                     _ -> return ()
                 let _ = xx17_20
                 return ()
                 xx18_21 <- dvCharsM
                 if id isI xx18_21
                  then return ()
                  else throwError (strMsg "not match")
                 case xx18_21 of
                     _ -> return ()
                 let _ = xx18_21
                 return ()
                 xx19_22 <- dvCharsM
                 if id isL xx19_22
                  then return ()
                  else throwError (strMsg "not match")
                 case xx19_22 of
                     _ -> return ()
                 let _ = xx19_22
                 return ()
                 xx20_23 <- dvCharsM
                 if id isL xx20_23
                  then return ()
                  else throwError (strMsg "not match")
                 case xx20_23 of
                     _ -> return ()
                 let _ = xx20_23
                 return ()
                 xx21_24 <- dvCharsM
                 if id isO xx21_24
                  then return ()
                  else throwError (strMsg "not match")
                 case xx21_24 of
                     _ -> return ()
                 let _ = xx21_24
                 return ()
                 xx22_25 <- dvCharsM
                 if id isN xx22_25
                  then return ()
                  else throwError (strMsg "not match")
                 case xx22_25 of
                     _ -> return ()
                 let _ = xx22_25
                 return ()
                 xx23_26 <- dvCharsM
                 if id isBar xx23_26
                  then return ()
                  else throwError (strMsg "not match")
                 case xx23_26 of
                     _ -> return ()
                 let _ = xx23_26
                 return ()
                 xx24_27 <- dvCharsM
                 if id isNL xx24_27
                  then return ()
                  else throwError (strMsg "not match")
                 case xx24_27 of
                     _ -> return ()
                 let _ = xx24_27
                 return ()
                 return (id nil)]
p_peg = msum [do _ <- dv_spacesM
                 d <- dv_definitionM
                 p <- dv_pegM
                 return (id cons d p),
              do return (id empty)]
p_definition = msum [do v <- dv_variableM
                        _ <- dv_spacesM
                        xx25_28 <- dvCharsM
                        if id isColon xx25_28
                         then return ()
                         else throwError (strMsg "not match")
                        case xx25_28 of
                            _ -> return ()
                        let _ = xx25_28
                        return ()
                        xx26_29 <- dvCharsM
                        if id isColon xx26_29
                         then return ()
                         else throwError (strMsg "not match")
                        case xx26_29 of
                            _ -> return ()
                        let _ = xx26_29
                        return ()
                        _ <- dv_spacesM
                        t <- dv_typM
                        _ <- dv_spacesM
                        xx27_30 <- dvCharsM
                        if id isEqual xx27_30
                         then return ()
                         else throwError (strMsg "not match")
                        case xx27_30 of
                            _ -> return ()
                        let _ = xx27_30
                        return ()
                        _ <- dv_spacesM
                        sel <- dv_selectionM
                        _ <- dv_spacesM
                        xx28_31 <- dvCharsM
                        if id isSemi xx28_31
                         then return ()
                         else throwError (strMsg "not match")
                        case xx28_31 of
                            _ -> return ()
                        let _ = xx28_31
                        return ()
                        return (id mkDef v t sel)]
p_selection = msum [do ex <- dv_expressionHsM
                       _ <- dv_spacesM
                       xx29_32 <- dvCharsM
                       if id isSlash xx29_32
                        then return ()
                        else throwError (strMsg "not match")
                       case xx29_32 of
                           _ -> return ()
                       let _ = xx29_32
                       return ()
                       _ <- dv_spacesM
                       sel <- dv_selectionM
                       return (id cons ex sel),
                    do ex <- dv_expressionHsM
                       return (id cons ex empty)]
p_expressionHs = msum [do e <- dv_expressionM
                          _ <- dv_spacesM
                          xx30_33 <- dvCharsM
                          if id isOpenWave xx30_33
                           then return ()
                           else throwError (strMsg "not match")
                          case xx30_33 of
                              _ -> return ()
                          let _ = xx30_33
                          return ()
                          _ <- dv_spacesM
                          h <- dv_hsExpM
                          _ <- dv_spacesM
                          xx31_34 <- dvCharsM
                          if id isCloseWave xx31_34
                           then return ()
                           else throwError (strMsg "not match")
                          case xx31_34 of
                              _ -> return ()
                          let _ = xx31_34
                          return ()
                          return (id mkExpressionHs e h)]
p_expression = msum [do l <- dv_nameLeafM
                        _ <- dv_spacesM
                        e <- dv_expressionM
                        return (id cons l e),
                     do return (id empty)]
p_nameLeaf = msum [do n <- dv_patM
                      xx32_35 <- dvCharsM
                      if id isColon xx32_35
                       then return ()
                       else throwError (strMsg "not match")
                      case xx32_35 of
                          _ -> return ()
                      let _ = xx32_35
                      return ()
                      l <- dv_leaf_M
                      return (id mkNameLeaf n l),
                   do n <- dv_patM
                      return (id mkNameLeaf n ctLeaf)]
p_pat = msum [do n <- dv_variableM
                 return (id strToPatQ n),
              do t <- dv_typM
                 _ <- dv_spacesM
                 ps <- dv_patsM
                 return (id conToPatQ t ps),
              do xx33_36 <- dvCharsM
                 if id isChon xx33_36
                  then return ()
                  else throwError (strMsg "not match")
                 case xx33_36 of
                     _ -> return ()
                 let _ = xx33_36
                 return ()
                 xx34_37 <- dvCharsM
                 if id const true xx34_37
                  then return ()
                  else throwError (strMsg "not match")
                 case xx34_37 of
                     _ -> return ()
                 let c = xx34_37
                 return ()
                 xx35_38 <- dvCharsM
                 if id isChon xx35_38
                  then return ()
                  else throwError (strMsg "not match")
                 case xx35_38 of
                     _ -> return ()
                 let _ = xx35_38
                 return ()
                 return (id charP c),
              do xx36_39 <- dvCharsM
                 if id isDQ xx36_39
                  then return ()
                  else throwError (strMsg "not match")
                 case xx36_39 of
                     _ -> return ()
                 let _ = xx36_39
                 return ()
                 s <- dv_stringLitM
                 xx37_40 <- dvCharsM
                 if id isDQ xx37_40
                  then return ()
                  else throwError (strMsg "not match")
                 case xx37_40 of
                     _ -> return ()
                 let _ = xx37_40
                 return ()
                 return (id stringP s)]
p_stringLit = msum [do d_41 <- get
                       flipMaybe dv_dqM
                       put d_41
                       xx38_42 <- dvCharsM
                       if const True xx38_42
                        then return ()
                        else throwError (strMsg "not match")
                       case xx38_42 of
                           _ -> return ()
                       let c = xx38_42
                       return ()
                       s <- dv_stringLitM
                       return (id cons c s),
                    do return (id empty)]
p_dq = msum [do xx39_43 <- dvCharsM
                if const True xx39_43
                 then return ()
                 else throwError (strMsg "not match")
                case xx39_43 of
                    '"' -> return ()
                    _ -> throwError (strMsg "not match")
                let '"' = xx39_43
                return ()
                return (id nil)]
p_pats = msum [do p <- dv_patM
                  ps <- dv_patsM
                  return (id cons p ps),
               do return (id empty)]
p_leaf_ = msum [do xx40_44 <- dvCharsM
                   if id isNot xx40_44
                    then return ()
                    else throwError (strMsg "not match")
                   case xx40_44 of
                       _ -> return ()
                   let _ = xx40_44
                   return ()
                   l <- dv_leafM
                   return (id notAfter l),
                do l <- dv_leafM
                   return (id here l)]
p_leaf = msum [do t <- dv_testM
                  return (id left t),
               do v <- dv_variableM
                  return (id right v)]
p_test = msum [do xx41_45 <- dvCharsM
                  if id isOpenBr xx41_45
                   then return ()
                   else throwError (strMsg "not match")
                  case xx41_45 of
                      _ -> return ()
                  let _ = xx41_45
                  return ()
                  h <- dv_hsExpM
                  xx42_46 <- dvCharsM
                  if id isCloseBr xx42_46
                   then return ()
                   else throwError (strMsg "not match")
                  case xx42_46 of
                      _ -> return ()
                  let _ = xx42_46
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
p_upper = msum [do xx43_47 <- dvCharsM
                   if id isUpper xx43_47
                    then return ()
                    else throwError (strMsg "not match")
                   case xx43_47 of
                       _ -> return ()
                   let u = xx43_47
                   return ()
                   return (id u)]
p_lower = msum [do xx44_48 <- dvCharsM
                   if id isLowerU xx44_48
                    then return ()
                    else throwError (strMsg "not match")
                   case xx44_48 of
                       _ -> return ()
                   let l = xx44_48
                   return ()
                   return (id l)]
p_digit = msum [do xx45_49 <- dvCharsM
                   if id isDigit xx45_49
                    then return ()
                    else throwError (strMsg "not match")
                   case xx45_49 of
                       _ -> return ()
                   let d = xx45_49
                   return ()
                   return (id d)]
p_spaces = msum [do _ <- dv_spaceM
                    _ <- dv_spacesM
                    return (id nil),
                 do return (id nil)]
p_space = msum [do xx46_50 <- dvCharsM
                   if id isSpace xx46_50
                    then return ()
                    else throwError (strMsg "not match")
                   case xx46_50 of
                       _ -> return ()
                   let _ = xx46_50
                   return ()
                   return (id nil)]
