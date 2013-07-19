data AB = A | B deriving Show
data S = Rec S AB | Atom AB deriving Show

testPackrat :: S
testPackrat = fromReturn $ drvS $ parse [A, B, B]

fromReturn (Return x _) = x

data Return a = Return a Derivs | LR Bool | Fail

data Derivs = Derivs {
	drvS :: Return S,
	chars :: Return AB
 }

parse :: [AB] -> Derivs
parse s = d
	where
	d = Derivs (ds d { drvS = LR False }) dab
	ds drv = case drvS drv of
		Return r rest -> case chars rest of
			Return B rest' -> Return (Rec r B) rest'
			_ -> Fail
		LR False -> case drvS d { drvS = ds drv { drvS = Fail } } of
			Return r rest -> case chars rest of
				Return B rest' -> Return (Rec r B) rest'
				_ -> Fail
			_ -> case chars d of
				Return A rest -> Return (Atom A) rest
				_ -> Fail
		_ -> case chars d of
			Return A rest -> Return (Atom A) rest
			_ -> Fail
	dab = case s of
		c : cs -> Return c $ parse cs
		_ -> Fail
