{-# LANGUAGE TemplateHaskell, PackageImports #-}

module Text.Papillon.List (
	listDec,
	optionalDec
) where

import Language.Haskell.TH
import Control.Applicative
import Control.Monad

{-

list, list1 :: (MonadPlus m, Applicative m) => m a -> m [a]
list p = list1 p `mplus` return []
list1 p = (:) <$> p <*> list p

-}

monadPlusN, mplusN, applicativeN, applyN, applyContN :: Bool -> Name
monadPlusN True = ''MonadPlus
monadPlusN False = mkName "MonadPlus"
applicativeN True = ''Applicative
applicativeN False = mkName "Applicative"
mplusN True = 'mplus
mplusN False = mkName "mplus"
applyN True = '(<$>)
applyN False = mkName "<$>"
applyContN True = '(<*>)
applyContN False = mkName "<*>"

m, a, p :: Name
m = mkName "m"
a = mkName "a"
p = mkName "p"

listDec :: Name -> Name -> Bool -> DecsQ
listDec list list1 th = sequence [
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
	vm = varT m
	returnEmpty = varE (mkName "return") `appE` listE []
	cons = conE $ mkName ":"
	app = varE $ applyN th
	next = varE $ applyContN th

{-

optional :: (MonadPlus m, Applicative m) => m a -> m (Maybe a)
optional p = (Just <$> p) `mplus` return Nothing

-}

optionalDec :: Name -> Bool -> DecsQ
optionalDec optionalN th = sequence [
	sigD optionalN $ mplusAndApp $ (varT m `appT` varT a) `arrT`
		(varT m `appT` (conT (mkName "Maybe") `appT` varT a)),
	funD optionalN $ (: []) $ flip (clause [varP p]) [] $ normalB $
		conE (mkName "Just") `app` varE p `mplusE` returnNothing
 ] where
	mplusAndApp = forallT [PlainTV m, PlainTV a] $ cxt [
		classP (monadPlusN th) [varT m],
		classP (applicativeN th) [varT m]
	 ]
	arrT f x = arrowT `appT` f `appT` x
	mplusE x = infixApp x (varE $ mplusN th)
	returnNothing = varE (mkName "return") `appE` conE (mkName "Nothing")
	app x = infixApp x (varE $ applyN th)
