{-# LANGUAGE PackageImports, FlexibleContexts #-}

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
mkPegFile :: a -> b -> c -> (a, b, c)
mkPegFile = (,,)

true :: Bool
true = True

charP :: Char -> PatQ
charP = litP . charL

isEqual, isSlash, isSemi,
	isColon, isOpenWave, isCloseWave, isLowerU, isNot, isChon :: Char -> Bool
isEqual = (== '=')
isSlash = (== '/')
isSemi = (== ';')
isColon = (== ':')
isOpenWave = (== '{')
isCloseWave = (== '}')
isLowerU c = isLower c || c == '_'
isNot = (== '!')
isChon = (== '\'')

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
              dv_prePeg :: (Result String),
              dv_afterPeg :: (Result String),
              dv_pap :: (Result Nil),
              dv_peg :: (Result Peg),
              dv_definition :: (Result Definition),
              dv_selection :: (Result Selection),
              dv_expressionHs :: (Result ExpressionHs),
              dv_expression :: (Result Expression),
              dv_nameLeaf :: (Result NameLeaf),
              dv_pat :: (Result PatQ),
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
          where d = Derivs pegFile prePeg afterPeg pap peg definition selection expressionHs expression nameLeaf pat pats leaf_ leaf test hsExp typ variable tvtail alpha upper lower digit spaces space char
                pegFile = runStateT p_pegFile d
                prePeg = runStateT p_prePeg d
                afterPeg = runStateT p_afterPeg d
                pap = runStateT p_pap d
                peg = runStateT p_peg d
                definition = runStateT p_definition d
                selection = runStateT p_selection d
                expressionHs = runStateT p_expressionHs d
                expression = runStateT p_expression d
                nameLeaf = runStateT p_nameLeaf d
                pat = runStateT p_pat d
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
dv_prePegM :: PackratM String
dv_afterPegM :: PackratM String
dv_papM :: PackratM Nil
dv_pegM :: PackratM Peg
dv_definitionM :: PackratM Definition
dv_selectionM :: PackratM Selection
dv_expressionHsM :: PackratM ExpressionHs
dv_expressionM :: PackratM Expression
dv_nameLeafM :: PackratM NameLeaf
dv_patM :: PackratM PatQ
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
dv_prePegM = StateT dv_prePeg
dv_afterPegM = StateT dv_afterPeg
dv_papM = StateT dv_pap
dv_pegM = StateT dv_peg
dv_definitionM = StateT dv_definition
dv_selectionM = StateT dv_selection
dv_expressionHsM = StateT dv_expressionHs
dv_expressionM = StateT dv_expression
dv_nameLeafM = StateT dv_nameLeaf
dv_patM = StateT dv_pat
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
p_prePeg :: PackratM String
p_afterPeg :: PackratM String
p_pap :: PackratM Nil
p_peg :: PackratM Peg
p_definition :: PackratM Definition
p_selection :: PackratM Selection
p_expressionHs :: PackratM ExpressionHs
p_expression :: PackratM Expression
p_nameLeaf :: PackratM NameLeaf
p_pat :: PackratM PatQ
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
p_pegFile = msum [do pp <- dv_prePegM
                     _ <- dv_papM
                     p <- dv_pegM
                     _ <- dv_spacesM
                     xx0_0 <- dvCharsM
                     let _ = xx0_0
                     if id isBar xx0_0
                      then return ()
                      else throwError (strMsg "not match")
                     xx1_1 <- dvCharsM
                     let _ = xx1_1
                     if id isCloseBr xx1_1
                      then return ()
                      else throwError (strMsg "not match")
                     xx2_2 <- dvCharsM
                     let _ = xx2_2
                     if id isNL xx2_2
                      then return ()
                      else throwError (strMsg "not match")
                     atp <- dv_afterPegM
                     return (id mkPegFile pp p atp)]
p_prePeg = msum [do d <- get
                    flipMaybe dv_papM
                    put d
                    xx3_3 <- dvCharsM
                    let c = xx3_3
                    if id const true xx3_3
                     then return ()
                     else throwError (strMsg "not match")
                    pp <- dv_prePegM
                    return (id cons c pp),
                 do return (id empty)]
p_afterPeg = msum [do xx4_4 <- dvCharsM
                      let c = xx4_4
                      if id const true xx4_4
                       then return ()
                       else throwError (strMsg "not match")
                      atp <- dv_afterPegM
                      return (id cons c atp),
                   do return (id empty)]
p_pap = msum [do xx5_5 <- dvCharsM
                 let _ = xx5_5
                 if id isNL xx5_5
                  then return ()
                  else throwError (strMsg "not match")
                 xx6_6 <- dvCharsM
                 let _ = xx6_6
                 if id isOpenBr xx6_6
                  then return ()
                  else throwError (strMsg "not match")
                 xx7_7 <- dvCharsM
                 let _ = xx7_7
                 if id isP xx7_7 then return () else throwError (strMsg "not match")
                 xx8_8 <- dvCharsM
                 let _ = xx8_8
                 if id isA xx8_8 then return () else throwError (strMsg "not match")
                 xx9_9 <- dvCharsM
                 let _ = xx9_9
                 if id isP xx9_9 then return () else throwError (strMsg "not match")
                 xx10_10 <- dvCharsM
                 let _ = xx10_10
                 if id isI xx10_10
                  then return ()
                  else throwError (strMsg "not match")
                 xx11_11 <- dvCharsM
                 let _ = xx11_11
                 if id isL xx11_11
                  then return ()
                  else throwError (strMsg "not match")
                 xx12_12 <- dvCharsM
                 let _ = xx12_12
                 if id isL xx12_12
                  then return ()
                  else throwError (strMsg "not match")
                 xx13_13 <- dvCharsM
                 let _ = xx13_13
                 if id isO xx13_13
                  then return ()
                  else throwError (strMsg "not match")
                 xx14_14 <- dvCharsM
                 let _ = xx14_14
                 if id isN xx14_14
                  then return ()
                  else throwError (strMsg "not match")
                 xx15_15 <- dvCharsM
                 let _ = xx15_15
                 if id isBar xx15_15
                  then return ()
                  else throwError (strMsg "not match")
                 xx16_16 <- dvCharsM
                 let _ = xx16_16
                 if id isNL xx16_16
                  then return ()
                  else throwError (strMsg "not match")
                 return (id nil)]
p_peg = msum [do _ <- dv_spacesM
                 d <- dv_definitionM
                 p <- dv_pegM
                 return (id cons d p),
              do return (id empty)]
p_definition = msum [do v <- dv_variableM
                        _ <- dv_spacesM
                        xx17_17 <- dvCharsM
                        let _ = xx17_17
                        if id isColon xx17_17
                         then return ()
                         else throwError (strMsg "not match")
                        xx18_18 <- dvCharsM
                        let _ = xx18_18
                        if id isColon xx18_18
                         then return ()
                         else throwError (strMsg "not match")
                        _ <- dv_spacesM
                        t <- dv_typM
                        _ <- dv_spacesM
                        xx19_19 <- dvCharsM
                        let _ = xx19_19
                        if id isEqual xx19_19
                         then return ()
                         else throwError (strMsg "not match")
                        _ <- dv_spacesM
                        sel <- dv_selectionM
                        _ <- dv_spacesM
                        xx20_20 <- dvCharsM
                        let _ = xx20_20
                        if id isSemi xx20_20
                         then return ()
                         else throwError (strMsg "not match")
                        return (id mkDef v t sel)]
p_selection = msum [do ex <- dv_expressionHsM
                       _ <- dv_spacesM
                       xx21_21 <- dvCharsM
                       let _ = xx21_21
                       if id isSlash xx21_21
                        then return ()
                        else throwError (strMsg "not match")
                       _ <- dv_spacesM
                       sel <- dv_selectionM
                       return (id cons ex sel),
                    do ex <- dv_expressionHsM
                       return (id cons ex empty)]
p_expressionHs = msum [do e <- dv_expressionM
                          _ <- dv_spacesM
                          xx22_22 <- dvCharsM
                          let _ = xx22_22
                          if id isOpenWave xx22_22
                           then return ()
                           else throwError (strMsg "not match")
                          _ <- dv_spacesM
                          h <- dv_hsExpM
                          _ <- dv_spacesM
                          xx23_23 <- dvCharsM
                          let _ = xx23_23
                          if id isCloseWave xx23_23
                           then return ()
                           else throwError (strMsg "not match")
                          return (id mkExpressionHs e h)]
p_expression = msum [do l <- dv_nameLeafM
                        _ <- dv_spacesM
                        e <- dv_expressionM
                        return (id cons l e),
                     do return (id empty)]
p_nameLeaf = msum [do n <- dv_patM
                      xx24_24 <- dvCharsM
                      let _ = xx24_24
                      if id isColon xx24_24
                       then return ()
                       else throwError (strMsg "not match")
                      l <- dv_leaf_M
                      return (id mkNameLeaf n l)]
p_pat = msum [do n <- dv_variableM
                 return (id strToPatQ n),
              do t <- dv_typM
                 _ <- dv_spacesM
                 ps <- dv_patsM
                 return (id conToPatQ t ps),
              do xx25_25 <- dvCharsM
                 let _ = xx25_25
                 if id isChon xx25_25
                  then return ()
                  else throwError (strMsg "not match")
                 xx26_26 <- dvCharsM
                 let c = xx26_26
                 if id const true xx26_26
                  then return ()
                  else throwError (strMsg "not match")
                 xx27_27 <- dvCharsM
                 let _ = xx27_27
                 if id isChon xx27_27
                  then return ()
                  else throwError (strMsg "not match")
                 return (id charP c)]
p_pats = msum [do p <- dv_patM
                  ps <- dv_patsM
                  return (id cons p ps),
               do return (id empty)]
p_leaf_ = msum [do xx28_28 <- dvCharsM
                   let _ = xx28_28
                   if id isNot xx28_28
                    then return ()
                    else throwError (strMsg "not match")
                   l <- dv_leafM
                   return (id notAfter l),
                do l <- dv_leafM
                   return (id here l)]
p_leaf = msum [do t <- dv_testM
                  return (id left t),
               do v <- dv_variableM
                  return (id right v)]
p_test = msum [do xx29_29 <- dvCharsM
                  let _ = xx29_29
                  if id isOpenBr xx29_29
                   then return ()
                   else throwError (strMsg "not match")
                  h <- dv_hsExpM
                  xx30_30 <- dvCharsM
                  let _ = xx30_30
                  if id isCloseBr xx30_30
                   then return ()
                   else throwError (strMsg "not match")
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
p_upper = msum [do xx31_31 <- dvCharsM
                   let u = xx31_31
                   if id isUpper xx31_31
                    then return ()
                    else throwError (strMsg "not match")
                   return (id u)]
p_lower = msum [do xx32_32 <- dvCharsM
                   let l = xx32_32
                   if id isLowerU xx32_32
                    then return ()
                    else throwError (strMsg "not match")
                   return (id l)]
p_digit = msum [do xx33_33 <- dvCharsM
                   let d = xx33_33
                   if id isDigit xx33_33
                    then return ()
                    else throwError (strMsg "not match")
                   return (id d)]
p_spaces = msum [do _ <- dv_spaceM
                    _ <- dv_spacesM
                    return (id nil),
                 do return (id nil)]
p_space = msum [do xx34_34 <- dvCharsM
                   let _ = xx34_34
                   if id isSpace xx34_34
                    then return ()
                    else throwError (strMsg "not match")
                   return (id nil)]
