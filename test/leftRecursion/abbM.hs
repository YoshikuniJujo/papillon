{-# LANGUAGE PackageImports, TupleSections #-}

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Applicative

data AB = A | B | C deriving Show
data S = Rec S AB | Atom AB deriving Show
-- data S = Rec AB S | Atom AB deriving Show

data Fail = Fail deriving Show

instance Error Fail where

type Return a = Either Fail (Either Bool a, Derivs)

data Derivs = Derivs {
	drvS :: Return S,
	chars :: Return AB
 }

testPackrat :: S
testPackrat = case drvS $ parse [A, B, B] of
	Right (Right r, _) -> r
	_ -> error "bad"

parse :: [AB] -> Derivs
parse s = d
	where
	d = Derivs ds dab
	ds = runStateT sm $ d { drvS = Right (Left False, d) }
	dab = flip runStateT d $ case s of
		c : cs -> do
			put $ parse cs
			return $ Right c
		_ -> throwError Fail

sm :: StateT Derivs (Either Fail) (Either Bool S)
sm = foldl1 mplus [ do
	sm' <- StateT drvS
	case sm' of
		Right s -> do
			mc <- StateT chars
			case mc of
				Right c -> return $ Right $ Rec s c
				_ -> throwError Fail
		Left False -> grow $ Left Fail
		Left True -> throwError Fail
			
 , do	mc <- StateT chars
	case mc of
		Right c -> return $ Right $ Atom c
		_ -> throwError Fail
 ]

grow :: Return S -> StateT Derivs (Either Fail) (Either Bool S)
grow x = do
	d <- get
	put d { drvS = x }
	(b, r) <- catchError ((True ,) <$> sm) $ const $ do
		xx <- StateT $ const x
		return (False, xx)
	d <- get
	if b then grow $ Right (r, d) else return r

setDrvS :: Return S -> Derivs -> Derivs
setDrvS s d = d { drvS = s }
