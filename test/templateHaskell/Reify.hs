{-# LANGUAGE TemplateHaskell #-}

module Reify where

import Language.Haskell.TH
import Control.Applicative

testReify :: Q [Con]
testReify = do
	DataConI _ (ConT n) _ _ <- reify 'True
	TyConI (DataD _ _ _ c _) <- reify n
	return c

noOthers :: Pat -> Q Bool
noOthers (LitP _) = return True
noOthers (VarP _) = return True
noOthers (TupP pats) = and <$> mapM noOthers pats
noOthers (UnboxedTupP pats) = and <$> mapM noOthers pats
-- noOthers
noOthers WildP = return True
