{-# LANGUAGE PackageImports #-}

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Applicative

data AB = A | B deriving Show
data S = Rec AB S | Atom AB deriving Show

-- X = S
-- S = a X | b

data Fail = Fail deriving Show
instance Error Fail where

type Result a = Either Fail (a, Derivs)

data Derivs = Derivs {
	getX :: Result S,
	getS :: Result S,
	chars :: Result AB
 } deriving Show

getParseResult :: Derivs -> S
getParseResult d = case getS d of
	Right (r, _) -> r
	_ -> error "parse error"

parse :: [AB] -> Derivs
parse abstr = d
	where
	d = Derivs x s c
	x = runStateT ruleX d
	s = runStateT ruleS d
	c = flip runStateT d $ do
		case abstr of
			ab : abs -> do
				put $ parse abs
				return ab
			_ -> throwError Fail

ruleX :: StateT Derivs (Either Fail) S
ruleX = StateT getS

ruleS :: StateT Derivs (Either Fail) S
ruleS = foldl1 mplus [do
	c <- StateT chars
	x <- StateT getX
	return $ Rec c x
 , do	c <- StateT chars
	return $ Atom c
 ]
