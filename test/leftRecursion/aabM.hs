{-# LANGUAGE PackageImports #-}

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error

data AB = A | B deriving Show
-- data S = Rec S AB | Atom AB deriving Show
data S = Rec AB S | Atom AB deriving Show

data FailOrLR = Fail | LR deriving Show

instance Error FailOrLR where

type Return a = Either FailOrLR (a, Derivs)

data Derivs = Derivs {
	drvS :: Return S,
	chars :: Return AB
 }

testPackrat :: S
testPackrat = case drvS $ parse [A, A, B] of
	Right (r, _) -> r
	_ -> error "bad"

parse :: [AB] -> Derivs
parse s = d
	where
	d = Derivs ds dab
	ds = runStateT sm d
	dab = flip runStateT d $ case s of
		c : cs -> do
			put $ parse cs
			return c
		_ -> throwError Fail

sm :: StateT Derivs (Either FailOrLR) S
sm = foldl1 mplus [ do
	c <- StateT chars
	sm' <- StateT drvS
	return $ Rec c sm', do
	c <- StateT chars
	return $ Atom c]
