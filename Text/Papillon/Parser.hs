{-# LANGUAGE PackageImports #-}

module Text.Papillon.Parser (
	Peg,
	Definition,
	NameLeaf,
	parse,
	dv_peg
) where

import GHC.Base
import GHC.Types
import Data.Char
import Control.Monad
import "monads-tf" Control.Monad.State.Class
import "transformers" Control.Monad.Trans.State.Lazy
import Data.Maybe
import Language.Haskell.TH

type Nil = ()
type Leaf = Either String ExR
type NameLeaf = (Name, Leaf)
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
	isColon, isOpenWave, isCloseWave, isLowerU :: Char -> Bool
isOpenBr = (== '[')
isCloseBr = (== ']')
isEqual = (== '=')
isSlash = (== '/')
isSemi = (== ';')
isColon = (== ':')
isOpenWave = (== '{')
isCloseWave = (== '}')
isLowerU c = isLower c || c == '_'

type PackratM = Control.Monad.Trans.State.Lazy.StateT Derivs
                                                      Data.Maybe.Maybe
type Result v = Data.Maybe.Maybe ((v, Derivs))
data Derivs
    = Derivs {dv_peg :: (Result Peg),
              dv_definition :: (Result Definition),
              dv_selection :: (Result Selection),
              dv_expressionHs :: (Result ExpressionHs),
              dv_expression :: (Result Expression),
              dv_nameLeaf :: (Result NameLeaf),
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
              dvChars :: (Result GHC.Types.Char)}
parse :: GHC.Base.String -> Derivs
parse s = d
          where d = Derivs peg definition selection expressionHs expression nameLeaf leaf test hsExp typ variable tvtail alpha upper lower digit spaces space char
                peg = runStateT p_peg d
                definition = runStateT p_definition d
                selection = runStateT p_selection d
                expressionHs = runStateT p_expressionHs d
                expression = runStateT p_expression d
                nameLeaf = runStateT p_nameLeaf d
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
                char = flip runStateT d (do c : s' <- GHC.Base.return s
                                            Control.Monad.State.Class.put (parse s')
                                            GHC.Base.return c)
dv_pegM :: PackratM Peg
dv_definitionM :: PackratM Definition
dv_selectionM :: PackratM Selection
dv_expressionHsM :: PackratM ExpressionHs
dv_expressionM :: PackratM Expression
dv_nameLeafM :: PackratM NameLeaf
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
dv_pegM = Control.Monad.Trans.State.Lazy.StateT dv_peg
dv_definitionM = Control.Monad.Trans.State.Lazy.StateT dv_definition
dv_selectionM = Control.Monad.Trans.State.Lazy.StateT dv_selection
dv_expressionHsM = Control.Monad.Trans.State.Lazy.StateT dv_expressionHs
dv_expressionM = Control.Monad.Trans.State.Lazy.StateT dv_expression
dv_nameLeafM = Control.Monad.Trans.State.Lazy.StateT dv_nameLeaf
dv_leafM = Control.Monad.Trans.State.Lazy.StateT dv_leaf
dv_testM = Control.Monad.Trans.State.Lazy.StateT dv_test
dv_hsExpM = Control.Monad.Trans.State.Lazy.StateT dv_hsExp
dv_typM = Control.Monad.Trans.State.Lazy.StateT dv_typ
dv_variableM = Control.Monad.Trans.State.Lazy.StateT dv_variable
dv_tvtailM = Control.Monad.Trans.State.Lazy.StateT dv_tvtail
dv_alphaM = Control.Monad.Trans.State.Lazy.StateT dv_alpha
dv_upperM = Control.Monad.Trans.State.Lazy.StateT dv_upper
dv_lowerM = Control.Monad.Trans.State.Lazy.StateT dv_lower
dv_digitM = Control.Monad.Trans.State.Lazy.StateT dv_digit
dv_spacesM = Control.Monad.Trans.State.Lazy.StateT dv_spaces
dv_spaceM = Control.Monad.Trans.State.Lazy.StateT dv_space
dvCharsM :: PackratM GHC.Types.Char
dvCharsM = Control.Monad.Trans.State.Lazy.StateT dvChars
p_peg :: PackratM Peg
p_definition :: PackratM Definition
p_selection :: PackratM Selection
p_expressionHs :: PackratM ExpressionHs
p_expression :: PackratM Expression
p_nameLeaf :: PackratM NameLeaf
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
p_peg = Control.Monad.msum [do _ <- dv_spacesM
                               d <- dv_definitionM
                               p <- dv_pegM
                               GHC.Base.return (id cons d p),
                            do GHC.Base.return (id empty)]
p_definition = Control.Monad.msum [do v <- dv_variableM
                                      _ <- dv_spacesM
                                      c <- dvCharsM
                                      if id isColon c
                                       then GHC.Base.return ()
                                       else GHC.Base.fail "not match"
                                      cc <- dvCharsM
                                      if id isColon cc
                                       then GHC.Base.return ()
                                       else GHC.Base.fail "not match"
                                      _ <- dv_spacesM
                                      t <- dv_typM
                                      _ <- dv_spacesM
                                      e <- dvCharsM
                                      if id isEqual e
                                       then GHC.Base.return ()
                                       else GHC.Base.fail "not match"
                                      _ <- dv_spacesM
                                      sel <- dv_selectionM
                                      _ <- dv_spacesM
                                      s <- dvCharsM
                                      if id isSemi s
                                       then GHC.Base.return ()
                                       else GHC.Base.fail "not match"
                                      GHC.Base.return (id mkDef v t sel)]
p_selection = Control.Monad.msum [do ex <- dv_expressionHsM
                                     _ <- dv_spacesM
                                     e <- dvCharsM
                                     if id isSlash e
                                      then GHC.Base.return ()
                                      else GHC.Base.fail "not match"
                                     _ <- dv_spacesM
                                     sel <- dv_selectionM
                                     GHC.Base.return (id cons ex sel),
                                  do ex <- dv_expressionHsM
                                     GHC.Base.return (id cons ex empty)]
p_expressionHs = Control.Monad.msum [do e <- dv_expressionM
                                        _ <- dv_spacesM
                                        o <- dvCharsM
                                        if id isOpenWave o
                                         then GHC.Base.return ()
                                         else GHC.Base.fail "not match"
                                        _ <- dv_spacesM
                                        h <- dv_hsExpM
                                        _ <- dv_spacesM
                                        c <- dvCharsM
                                        if id isCloseWave c
                                         then GHC.Base.return ()
                                         else GHC.Base.fail "not match"
                                        GHC.Base.return (id mkExpressionHs e h)]
p_expression = Control.Monad.msum [do l <- dv_nameLeafM
                                      _ <- dv_spacesM
                                      e <- dv_expressionM
                                      GHC.Base.return (id cons l e),
                                   do GHC.Base.return (id empty)]
p_nameLeaf = Control.Monad.msum [do n <- dv_variableM
                                    c <- dvCharsM
                                    if id isColon c
                                     then GHC.Base.return ()
                                     else GHC.Base.fail "not match"
                                    l <- dv_leafM
                                    GHC.Base.return (id mkNameLeaf n l)]
p_leaf = Control.Monad.msum [do t <- dv_testM
                                GHC.Base.return (id left t),
                             do v <- dv_variableM
                                GHC.Base.return (id right v)]
p_test = Control.Monad.msum [do o <- dvCharsM
                                if id isOpenBr o
                                 then GHC.Base.return ()
                                 else GHC.Base.fail "not match"
                                h <- dv_hsExpM
                                c <- dvCharsM
                                if id isCloseBr c
                                 then GHC.Base.return ()
                                 else GHC.Base.fail "not match"
                                GHC.Base.return (id getEx h)]
p_hsExp = Control.Monad.msum [do v <- dv_variableM
                                 _ <- dv_spacesM
                                 h <- dv_hsExpM
                                 GHC.Base.return (id apply v h),
                              do v <- dv_variableM
                                 GHC.Base.return (id toExp v)]
p_typ = Control.Monad.msum [do u <- dv_upperM
                               t <- dv_tvtailM
                               GHC.Base.return (id cons u t)]
p_variable = Control.Monad.msum [do l <- dv_lowerM
                                    t <- dv_tvtailM
                                    GHC.Base.return (id cons l t)]
p_tvtail = Control.Monad.msum [do a <- dv_alphaM
                                  t <- dv_tvtailM
                                  GHC.Base.return (id cons a t),
                               do GHC.Base.return (id empty)]
p_alpha = Control.Monad.msum [do u <- dv_upperM
                                 GHC.Base.return (id u),
                              do l <- dv_lowerM
                                 GHC.Base.return (id l),
                              do d <- dv_digitM
                                 GHC.Base.return (id d)]
p_upper = Control.Monad.msum [do u <- dvCharsM
                                 if id isUpper u
                                  then GHC.Base.return ()
                                  else GHC.Base.fail "not match"
                                 GHC.Base.return (id u)]
p_lower = Control.Monad.msum [do l <- dvCharsM
                                 if id isLowerU l
                                  then GHC.Base.return ()
                                  else GHC.Base.fail "not match"
                                 GHC.Base.return (id l)]
p_digit = Control.Monad.msum [do d <- dvCharsM
                                 if id isDigit d
                                  then GHC.Base.return ()
                                  else GHC.Base.fail "not match"
                                 GHC.Base.return (id d)]
p_spaces = Control.Monad.msum [do _ <- dv_spaceM
                                  _ <- dv_spacesM
                                  GHC.Base.return (id nil),
                               do GHC.Base.return (id nil)]
p_space = Control.Monad.msum [do l <- dvCharsM
                                 if id isSpace l
                                  then GHC.Base.return ()
                                  else GHC.Base.fail "not match"
                                 GHC.Base.return (id nil)]
