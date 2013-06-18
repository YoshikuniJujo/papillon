{-# LANGUAGE PackageImports #-}

module Text.Papillon.Parser (
	Peg,
	Definition,
	NameLeaf,
	parse,
	dv_peg,
	Leaf_(..)
) where

import Data.Char
import Control.Monad
import "monads-tf" Control.Monad.State
import Language.Haskell.TH

type Nil = ()
type Leaf = Either String ExR
data Leaf_ = NotAfter Leaf | Here Leaf
notAfter, here :: Leaf -> Leaf_
notAfter = NotAfter
here = Here
type NameLeaf = (Name, Leaf_)
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
mkNameLeaf :: String -> b -> (Name, b)
mkNameLeaf x y = (mkName x, y)
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

isOpenBr, isCloseBr, isEqual, isSlash, isSemi,
	isColon, isOpenWave, isCloseWave, isLowerU, isNot :: Char -> Bool
isOpenBr = (== '[')
isCloseBr = (== ']')
isEqual = (== '=')
isSlash = (== '/')
isSemi = (== ';')
isColon = (== ':')
isOpenWave = (== '{')
isCloseWave = (== '}')
isLowerU c = isLower c || c == '_'
isNot = (== '!')

type PackratM = StateT Derivs Maybe
type Result v = Maybe ((v, Derivs))
data Derivs
    = Derivs {dv_peg :: (Result Peg),
              dv_definition :: (Result Definition),
              dv_selection :: (Result Selection),
              dv_expressionHs :: (Result ExpressionHs),
              dv_expression :: (Result Expression),
              dv_nameLeaf :: (Result NameLeaf),
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
          where d = Derivs peg definition selection expressionHs expression nameLeaf leaf_ leaf test hsExp typ variable tvtail alpha upper lower digit spaces space char
                peg = runStateT p_peg d
                definition = runStateT p_definition d
                selection = runStateT p_selection d
                expressionHs = runStateT p_expressionHs d
                expression = runStateT p_expression d
                nameLeaf = runStateT p_nameLeaf d
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
                char = flip runStateT d (do c : s' <- return s
                                            put (parse s')
                                            return c)
dv_pegM :: PackratM Peg
dv_definitionM :: PackratM Definition
dv_selectionM :: PackratM Selection
dv_expressionHsM :: PackratM ExpressionHs
dv_expressionM :: PackratM Expression
dv_nameLeafM :: PackratM NameLeaf
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
dv_pegM = StateT dv_peg
dv_definitionM = StateT dv_definition
dv_selectionM = StateT dv_selection
dv_expressionHsM = StateT dv_expressionHs
dv_expressionM = StateT dv_expression
dv_nameLeafM = StateT dv_nameLeaf
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
p_peg :: PackratM Peg
p_definition :: PackratM Definition
p_selection :: PackratM Selection
p_expressionHs :: PackratM ExpressionHs
p_expression :: PackratM Expression
p_nameLeaf :: PackratM NameLeaf
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
p_peg = msum [do _ <- dv_spacesM
                 d <- dv_definitionM
                 p <- dv_pegM
                 return (id cons d p),
              do return (id empty)]
p_definition = msum [do v <- dv_variableM
                        _ <- dv_spacesM
                        c <- dvCharsM
                        if id isColon c then return () else fail "not match"
                        cc <- dvCharsM
                        if id isColon cc then return () else fail "not match"
                        _ <- dv_spacesM
                        t <- dv_typM
                        _ <- dv_spacesM
                        e <- dvCharsM
                        if id isEqual e then return () else fail "not match"
                        _ <- dv_spacesM
                        sel <- dv_selectionM
                        _ <- dv_spacesM
                        s <- dvCharsM
                        if id isSemi s then return () else fail "not match"
                        return (id mkDef v t sel)]
p_selection = msum [do ex <- dv_expressionHsM
                       _ <- dv_spacesM
                       e <- dvCharsM
                       if id isSlash e then return () else fail "not match"
                       _ <- dv_spacesM
                       sel <- dv_selectionM
                       return (id cons ex sel),
                    do ex <- dv_expressionHsM
                       return (id cons ex empty)]
p_expressionHs = msum [do e <- dv_expressionM
                          _ <- dv_spacesM
                          o <- dvCharsM
                          if id isOpenWave o then return () else fail "not match"
                          _ <- dv_spacesM
                          h <- dv_hsExpM
                          _ <- dv_spacesM
                          c <- dvCharsM
                          if id isCloseWave c then return () else fail "not match"
                          return (id mkExpressionHs e h)]
p_expression = msum [do l <- dv_nameLeafM
                        _ <- dv_spacesM
                        e <- dv_expressionM
                        return (id cons l e),
                     do return (id empty)]
p_nameLeaf = msum [do n <- dv_variableM
                      c <- dvCharsM
                      if id isColon c then return () else fail "not match"
                      l <- dv_leaf_M
                      return (id mkNameLeaf n l)]
p_leaf_ = msum [do n <- dvCharsM
                   if id isNot n then return () else fail "not match"
                   l <- dv_leafM
                   return (id notAfter l),
                do l <- dv_leafM
                   return (id here l)]
p_leaf = msum [do t <- dv_testM
                  return (id left t),
               do v <- dv_variableM
                  return (id right v)]
p_test = msum [do o <- dvCharsM
                  if id isOpenBr o then return () else fail "not match"
                  h <- dv_hsExpM
                  c <- dvCharsM
                  if id isCloseBr c then return () else fail "not match"
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
p_upper = msum [do u <- dvCharsM
                   if id isUpper u then return () else fail "not match"
                   return (id u)]
p_lower = msum [do l <- dvCharsM
                   if id isLowerU l then return () else fail "not match"
                   return (id l)]
p_digit = msum [do d <- dvCharsM
                   if id isDigit d then return () else fail "not match"
                   return (id d)]
p_spaces = msum [do _ <- dv_spaceM
                    _ <- dv_spacesM
                    return (id nil),
                 do return (id nil)]
p_space = msum [do l <- dvCharsM
                   if id isSpace l then return () else fail "not match"
                   return (id nil)]
