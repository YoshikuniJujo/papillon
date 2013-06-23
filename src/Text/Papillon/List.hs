{-# LANGUAGE TemplateHaskell, PackageImports #-}

module Text.Papillon.List (
	listDec
) where

import Language.Haskell.TH
import Control.Applicative
import Control.Monad
-- import "monads-tf" Control.Monad.State

{-

list, list1 :: MonadPlus m => m a -> m [a]
list p = list1 p `mplus` return []
list1 p = (:) <$> p <*> list p

-}

monadPlusN, mplusN, applicativeN :: Bool -> Name
monadPlusN True = ''MonadPlus
monadPlusN False = mkName "MonadPlus"
applicativeN True = ''Applicative
applicativeN False = mkName "Applicative"
mplusN True = 'mplus
mplusN False = mkName "mplus"

listDec :: Bool -> DecsQ
listDec th = sequence [
	sigD list $ forallT [PlainTV m, PlainTV a]
		(cxt [classP (monadPlusN th) [vm], classP (applicativeN th) [vm]]) $
		arrowT	`appT` (varT m `appT` varT a)
			`appT` (varT m `appT` (listT `appT` varT a)),
	sigD list1 $ forallT [PlainTV m, PlainTV a]
		(cxt [classP (monadPlusN th) [vm], classP (applicativeN th) [vm]]) $
		arrowT	`appT` (varT m `appT` varT a)
			`appT` (varT m `appT` (listT `appT` varT a)),
	funD list $ (: []) $ flip (clause [varP p]) [] $ normalB $
		infixApp (varE list1 `appE` varE p) (varE $ mplusN th) returnEmpty,
	funD list1 $ (: []) $ flip (clause [varP p]) [] $ normalB $
		infixApp (infixApp cons app (varE p)) next (varE list `appE` varE p)
 ] where
	list = mkName "list"
	list1 = mkName "list1"
	vm = varT m
	m = mkName "m"
	a = mkName "a"
	p = mkName "p"
	returnEmpty = varE (mkName "return") `appE` listE []
	cons = conE $ mkName ":"
	app = varE $ mkName "<$>"
	next = varE $ mkName "<*>"
