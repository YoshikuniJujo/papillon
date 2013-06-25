{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Data

data Derivs = Derivs {
	str :: String,
	int :: Int,
	chr :: Char
 }

main :: IO ()
main = do
	print $ $(getGetter 1) (Derivs "hoge" 3 'c')
