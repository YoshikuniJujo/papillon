{-# LANGUAGE PackageImports, TupleSections #-}

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Applicative

data AB = A | B deriving Show
data S = Rec S AB | Atom AB deriving Show

-- X = S
-- S = X b | a

data Fail = Fail | LR deriving Show
instance Error Fail where

type Result a = Either Fail (a, Derivs)

data Derivs = Derivs {
	getX :: Result S,
	getS :: Result S,
	chars :: Result AB
 } deriving Show

parse :: [AB] -> Derivs
parse abstr = d
	where
	d = Derivs x s c
	x = runStateT ruleX d { getX = Left LR }
	s = runStateT ruleS d { getS = Left LR }
	c = flip runStateT d $ do
		case abstr of
			ab : abs -> do
				put $ parse abs
				return ab
			_ -> throwError Fail

ruleX :: StateT Derivs (Either Fail) S
ruleX = catchError (StateT getS) $ \e -> case e of
-- ruleX = catchError ruleS $ \e -> case e of
{-
	LR -> grow ruleX
		(\mx d -> d { getX = mx `runStateT` d })
		(throwError Fail)
-}
	_ -> throwError Fail

ruleS :: StateT Derivs (Either Fail) S
ruleS = foldl1 mplus [ catchError (do
	s <- StateT getX
--	s <- ruleX
	c <- StateT chars
	return $ Rec s c) $ \e -> case e of
{-
		LR -> grow ruleS
			(\mx d -> d { getS = mx `runStateT` d })
			(throwError Fail)
-}
		_ -> throwError Fail
 , do	c <- StateT chars
	return $ Atom c
 ]

grow :: (MonadError m, MonadState m, Functor m) =>
	m a -> (m a -> StateType m -> StateType m) -> m a -> m a
grow action update mx = do
	modify $ update mx
	(b, r) <- catchError ((True ,) <$> action) $ const $
		(False ,) <$> mx
	if b then grow action update (return r) else return r

getParseResult :: Derivs -> S
getParseResult d = case getS d of
	Right (r, _) -> r
	_ -> error "parse error"
