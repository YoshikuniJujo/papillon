import Data.Maybe

data AB = A | B deriving Show
data S = Rec AB S | Atom AB deriving Show

s :: [AB] -> S
s [B] = Atom B
s (A : abs) = Rec A $ s abs

--

testPackrat :: S
testPackrat = fst $ fromJust $ drvS $ parse [A, A, B]

data Derivs = Derivs {
	drvS :: Maybe (S, Derivs),
	chars :: Maybe (AB, Derivs)
 }

parse :: [AB] -> Derivs
parse s = d
	where
	d = Derivs ds dab
	ds = case chars d of
		Just (A, rest) -> case drvS rest of
			Just (r, rest) -> Just (Rec A r, rest)
			_ -> case chars d of
				Just (B, rest) -> Just (Atom B, rest)
				_ -> Nothing
		_ -> case chars d of
			Just (B, rest) -> Just (Atom B, rest)
			_ -> Nothing
	dab = case s of
		c : cs -> Just (c, parse cs)
		_ -> Nothing
