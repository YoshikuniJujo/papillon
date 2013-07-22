import Language.Haskell.TH

putLeftRight :: Int -> ExpQ -> ExpQ
putLeftRight 0 ex = leftE `appE` ex
putLeftRight n ex = rightE `appE` putLeftRight (n - 1) ex

rightE, leftE :: ExpQ
rightE = conE $ mkName "Right"
leftE = conE $ mkName "Left"
