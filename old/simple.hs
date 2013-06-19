{-# LANGUAGE PackageImports #-}

import "monads-tf" Control.Monad.State
import Data.Char

data Derivs = Derivs {
	dvSome :: Result Char,
	dvChars :: Result Char
 } deriving Show

type Result v = Maybe (v, Derivs)
type PMonad = StateT Derivs Maybe

dvSomeM :: PMonad Char
dvSomeM = StateT dvSome

dvCharsM :: PMonad Char
dvCharsM = StateT dvChars

parse :: String -> Derivs
parse s = d where
	d = Derivs some chr
	some = runStateT pSome d
	chr = flip runStateT d $ do
		c : s' <- return s
		put $ parse s'
		return c

pSome :: PMonad Char
pSome = do
	d <- dvCharsM
	if isDigit d
	then return d
	else fail "not match"
