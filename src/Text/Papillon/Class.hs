{-# LANGUAGE TypeFamilies, TemplateHaskell #-}

module Text.Papillon.Class (
--	Source(..),
	classSourceQ
) where

import Language.Haskell.TH

{-
class Source sl where
	type Token sl
	getToken :: sl -> Maybe (Token sl, sl)

class SourceList c where
	listToken :: [c] -> Maybe (c, [c])

instance SourceList Char where
	listToken (c : s) = Just (c, s)
	listToken _ = Nothing

instance (SourceList c) => Source [c] where
	type Token [c] = c
	getToken = listToken
-}

classSourceQ :: Bool -> DecsQ
classSourceQ th = sequence
	[classS th, classSL th, instanceSLC th, instanceSrcStr th]

maybeN, nothingN, justN, consN, charN :: Bool -> Name
maybeN True = ''Maybe
maybeN False = mkName "Maybe"
nothingN True = 'Nothing
nothingN False = mkName "Nothing"
justN True = 'Just
justN False = mkName "Just"
consN True = '(:)
consN False = mkName ":"
charN True = ''Char
charN False = mkName "Char"

classS, classSL, instanceSLC, instanceSrcStr :: Bool -> DecQ
classS th = classD (cxt []) source [PlainTV sl] [] [
	familyNoKindD typeFam tokenN [PlainTV sl],
	sigD getTokenN $ arrowT `appT` varT sl `appT`
		(conT (maybeN th) `appT` tupleBody)
 ] where
	sl = mkName "sl"
	tupleBody = tupleT 2
		`appT` (conT tokenN `appT` varT sl)
		`appT` varT sl

classSL th = classD (cxt []) sourceList [PlainTV c] [] [
	sigD listTokenN $ arrowT `appT` (listT `appT` varT c) `appT`
		(conT (maybeN th) `appT` tupleBody)
 ] where
	c = mkName "c"
	tupleBody = tupleT 2 `appT` varT c `appT` (listT `appT` varT c)

source, sourceList, listTokenN, tokenN, getTokenN :: Name
sourceList = mkName "SourceList"
listTokenN = mkName "listToken"
source = mkName "Source"
tokenN = mkName "Token"
getTokenN = mkName "getToken"

instanceSLC th = instanceD (cxt []) (conT sourceList `appT` conT (charN th)) [
	funD listTokenN [
		clause [infixP (varP c) (consN th) (varP s)]
			(normalB $ conE (justN th) `appE` tupleBody) [],
		clause [wildP] (normalB $ conE $ nothingN th) []
	 ]
 ] where
	c = mkName "c"
	s = mkName "s"
	tupleBody = tupE [varE c, varE s]

instanceSrcStr _ =
	instanceD (cxt [classP sourceList [varT c]]) (conT source `appT` listC) [
		tySynInstD tokenN [listC] $ varT c,
		valD (varP getTokenN) (normalB $ varE listTokenN) []
	 ]
	where
	c = mkName "c"
	listC = listT `appT` varT c
