import Language.Haskell.TH

putLeftRight :: Int -> ExpQ -> ExpQ
putLeftRight 0 ex = leftE `appE` ex
putLeftRight n ex = rightE `appE` putLeftRight (n - 1) ex

rightE, leftE :: ExpQ
rightE = conE $ mkName "Right"
leftE = conE $ mkName "Left"

reduce :: Either Int (Either String (Either Char ()))
	-> Either Int (Either String Char)
reduce (Right (Right (Left c))) = Right $ Right c
reduce (Right (Left s)) = Right $ Left s
reduce (Left i) = Left i

data DotList x = x :-: (Either (DotList x) x) deriving Show
