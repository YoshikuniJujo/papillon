{-# LANGUAGE PackageImports, TupleSections #-}

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Applicative

data AB = A | B deriving Show
data S = Rec S AB | Atom AB deriving Show

-- S = S b | a

data Fail = Fail | LR deriving Show
instance Error Fail where

type Result a = Either Fail (a, Derivs)

data Derivs = Derivs {
	getS :: Result S,
	chars :: Result AB,
	position :: Integer
 } deriving Show

parse :: Integer -> [AB] -> Derivs
parse p abstr = d
	where
	d = Derivs s c p
	s = runStateT ruleS d { getS = Left LR }
	c = flip runStateT d $ do
		case abstr of
			ab : abs -> do
				put $ parse (p + 1) abs
				return ab
			_ -> throwError Fail

ruleS :: StateT Derivs (Either Fail) S
ruleS = foldl1 mplus [
   do	catchError (do
		s <- StateT getS
		c <- StateT chars
		case c of
			B -> return $ Rec s c
			_ -> throwError Fail) $
		\e -> case e of
			LR -> grow ruleS
				(\x d -> d { getS = x })
				(Left Fail)
			_ -> throwError Fail
 , do	c <- StateT chars
	case c of
		A -> return $ Atom A
		_ -> throwError Fail
 ]

type PMonad = StateT Derivs (Either Fail)

{-
grow :: (MonadError m, MonadState m, Functor m) =>
	m a -> (m a -> StateType m -> StateType m) -> m a -> m a
-}

grow :: PMonad a -> (Either Fail (a, Derivs) -> Derivs -> Derivs)
	-> Either Fail (a, Derivs) -> PMonad a
grow action update x = do
	d0 <- get
	modify $ update x
	(b, r) <- flip catchError
		(const $ (False ,) <$> StateT (const x)) $ ((True ,) <$>) $ do
			a <- action
			p' <- gets position
			case x of
				Right (_, d) ->
					if position d >= p'
					then throwError Fail
					else return a
				_ -> return a
	d1 <- get
	put d0
	if b then grow action update (Right (r, d1)) else StateT $ const x

getParseResult :: Derivs -> S
getParseResult d = case getS d of
	Right (r, _) -> r
	_ -> error "parse error"
