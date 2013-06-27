{-# LANGUAGE TypeFamilies, TemplateHaskell #-}

module Text.Papillon.Class (
--	Source(..),
	classSourceQ
) where

import Language.Haskell.TH

classSourceQ :: Bool -> DecsQ
classSourceQ th = sequence [classS th, classSL th, instanceSrcStr th,
	instanceShowListPosPos, instanceSLC th]

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

source, sourceList, listTokenN, tokenN, getTokenN, posN, updatePosN,
	listPosN, listUpdatePosN, listShowPosN, initialPosN, listInitialPosN
	:: Name
sourceList = mkName "SourceList"
listTokenN = mkName "listToken"
source = mkName "Source"
tokenN = mkName "Token"
getTokenN = mkName "getToken"
posN = mkName "Pos"
updatePosN = mkName "updatePos"
listPosN = mkName "ListPos"
listUpdatePosN = mkName "listUpdatePos"
listShowPosN = mkName "listShowPos"
initialPosN = mkName "initialPos"
listInitialPosN = mkName "listInitialPos"

classS, classSL, instanceSLC, instanceSrcStr :: Bool -> DecQ

{-
class Source sl where
	type Token sl
	data Pos sl
	getToken :: sl -> Maybe (Token sl, sl)
	initialPos :: Pos sl
	updatePos :: Token sl -> Pos sl -> Pos sl
-}

classS th = classD (cxt []) source [PlainTV sl] [] [
	familyNoKindD typeFam tokenN [PlainTV sl],
	familyNoKindD dataFam posN [PlainTV sl],
	sigD getTokenN $ arrowT `appT` varT sl `appT`
		(conT (maybeN th) `appT` tupleBody),
	sigD initialPosN $ conT posN `appT` varT sl,
	sigD updatePosN $ arrowT
		`appT` (conT tokenN `appT` varT sl)
		`appT` (arrowT
			`appT` (conT posN `appT` varT sl)
			`appT` (conT posN `appT` varT sl))
 ] where
	sl = mkName "sl"
	tupleBody = tupleT 2
		`appT` (conT tokenN `appT` varT sl)
		`appT` varT sl

{-
class SourceList c where
	data ListPos c
	listToken :: [c] -> Maybe (c, [c])
	listInitialPos :: ListPos c
	listUpdatePos :: c -> ListPos c -> ListPos c
	listShowPos :: ListPos c -> String
-}

classSL th = classD (cxt []) sourceList [PlainTV c] [] [
	familyNoKindD dataFam listPosN [PlainTV c],
	sigD listTokenN $ arrowT `appT` (listT `appT` varT c) `appT`
		(conT (maybeN th) `appT` tupleBody),
	sigD listInitialPosN $ conT listPosN `appT` varT c,
	sigD listUpdatePosN $ arrowT
		`appT` varT c
		`appT` (arrowT
			`appT` (conT listPosN `appT` varT c)
			`appT` (conT listPosN `appT` varT c)),
	sigD listShowPosN $ arrowT
		`appT` (conT listPosN `appT` varT c)
		`appT` conT (mkName "String")
 ] where
	c = mkName "c"
	tupleBody = tupleT 2 `appT` varT c `appT` (listT `appT` varT c)

{-
instance (SourceList c) => Source [c] where
	type Token [c] = c
	newtype Pos [c] = ListPos (ListPos c)
	getToken = listToken
	initialPos = ListPos listInitialPos
	updatePos c (ListPos p) = ListPos (listUpdatePos c p)
-}

instanceSrcStr _ =
	instanceD (cxt [classP sourceList [varT c]]) (conT source `appT` listC) [
		tySynInstD tokenN [listC] $ varT c,
		flip (newtypeInstD (cxt []) posN [listC]) [] $
			normalC listPosN [strictType notStrict $
				conT listPosN `appT` varT c],
		valD (varP getTokenN) (normalB $ varE listTokenN) [],
		flip (valD $ varP initialPosN) [] $ normalB $
			conE listPosN `appE` varE listInitialPosN,
		funD updatePosN $ (: []) $ flip (clause [pc, lp]) [] $ normalB $
			conE listPosN `appE`
				(varE listUpdatePosN `appE` varE c `appE` varE p)
	 ]
	where
	c = mkName "c"
	p = mkName "p"
	pc = varP c
	lp = conP listPosN [varP p]
	listC = listT `appT` varT c

{-

instance Show (ListPos a) => Show (Pos [a]) where
	show (ListPos x) = "ListPos " ++ show x

-}

instanceShowListPosPos :: DecQ
instanceShowListPosPos = instanceD (cxt [cxtShowListPos]) decType [body]
	where
	cxtShowListPos = classP (mkName "Show")
		[conT (mkName "ListPos") `appT` varT (mkName "a")]
	decType = conT (mkName "Show") `appT`
		(conT (mkName "Pos") `appT` (listT  `appT` varT (mkName "a")))
	body = funD (mkName "show") $ (: []) $ flip (clause [patListPos]) [] $
		normalB $ addParens $ infixApp
			(litE $ stringL "ListPos (")
			(varE $ mkName "++") $ infixApp
				(varE (mkName "show") `appE` varE (mkName "x"))
				(varE $ mkName "++")
				(litE $ stringL ")")
	patListPos = conP (mkName "ListPos") [varP $ mkName "x"]
	addParens str = infixApp
		(litE $ stringL "(")
		(varE $ mkName "++") $ infixApp
			str
			(varE $ mkName "++")
			(litE $ stringL ")")

{-
instance SourceList Char where
	newtype ListPos Char = CharPos (Int, Int)
	listToken (c : s) = Just (c, s)
	listToken _ = Nothing
	listInitialPos = CharPos (1, 1)
	listUpdatePos '\n' (CharPos (y, x)) = CharPos (y + 1, 0)
	listUpdatePos '\t' (CharPOs (y, x)) = CharPos (y, x + 8)
	listUpdatePos _ (CharPos (y, x)) = CharPos (y, x + 1)
	listShowPos (CharPos p)= show p
-}

instanceSLC th = instanceD (cxt []) (conT sourceList `appT` conT (charN th)) [
--	tySynInstD listPosN [conT $ charN th] $ tupleT 2 `appT` int `appT` int,
	newtypeInstD (cxt []) listPosN [conT $ charN th] (
		normalC (mkName "CharPos") [
			strictType notStrict $ tupleT 2
				`appT` conT (mkName "Int")
				`appT` conT (mkName "Int")]
	 ) [mkName "Show"],
	funD listTokenN [
		clause [infixP (varP c) (consN th) (varP s)]
			(normalB $ conE (justN th) `appE` tupleBody) [],
		clause [wildP] (normalB $ conE $ nothingN th) []
	 ],
	flip (valD $ varP listInitialPosN) [] $ normalB $
		conE (mkName "CharPos") `appE` tupE [one, one],
	funD listUpdatePosN [
		flip (clause [litP $ charL '\n', pCharPos [tupP [varP y, wildP]]]) [] $
			normalB $ eCharPos `appE` tupE [
				infixApp (varE y) plus one, zero],
		flip (clause [wildP, pCharPos [tupP [varP y, varP x]]]) [] $
			normalB $ eCharPos `appE` tupE [
				varE y, infixApp (varE x) plus one]
	 ],
	funD listShowPosN [
		flip (clause [pCharPos [varP pos]]) [] $ normalB $
			varE (mkName "show") `appE` varE pos
	 ]
 ] where
	c = mkName "c"
	s = mkName "s"
	y = mkName "y"
	x = mkName "x"
	pos = mkName "pos"
	tupleBody = tupE [varE c, varE s]
--	int = conT $ mkName "Int"
	one = litE $ integerL 1
	zero = litE $ integerL 0
	plus = varE $ mkName "+"
	charPosN = mkName "CharPos"
	eCharPos = conE charPosN
	pCharPos = conP charPosN
