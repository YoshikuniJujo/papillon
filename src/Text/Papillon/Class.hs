{-# LANGUAGE TypeFamilies, TemplateHaskell #-}

module Text.Papillon.Class (
--	Source(..),
	classSourceQ
) where

import Language.Haskell.TH

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

source, sourceList, listTokenN, tokenN, getTokenN, posN, updatePosN, showPosN,
	listPosN, listUpdatePosN, listShowPosN :: Name
sourceList = mkName "SourceList"
listTokenN = mkName "listToken"
source = mkName "Source"
tokenN = mkName "Token"
getTokenN = mkName "getToken"
posN = mkName "Pos"
updatePosN = mkName "updatePos"
showPosN = mkName "showPos"
listPosN = mkName "ListPos"
listUpdatePosN = mkName "listUpdatePos"
listShowPosN = mkName "listShowPos"

classS, classSL, instanceSLC, instanceSrcStr :: Bool -> DecQ

{-
class Source sl where
	type Token sl
	type Pos sl
	getToken :: sl -> Maybe (Token sl, sl)
	updatePos :: Token sl -> Pos sl -> Pos sl
	showPos :: Pos sl -> String
-}

classS th = classD (cxt []) source [PlainTV sl] [] [
	familyNoKindD typeFam tokenN [PlainTV sl],
	familyNoKindD typeFam posN [PlainTV sl],
	sigD getTokenN $ arrowT `appT` varT sl `appT`
		(conT (maybeN th) `appT` tupleBody),
	sigD updatePosN $ arrowT
		`appT` (conT tokenN `appT` varT sl)
		`appT` (arrowT
			`appT` (conT posN `appT` varT sl)
			`appT` (conT posN `appT` varT sl)),
	sigD showPosN $ arrowT
		`appT` (conT posN `appT` varT sl)
		`appT` conT (mkName "String")
 ] where
	sl = mkName "sl"
	tupleBody = tupleT 2
		`appT` (conT tokenN `appT` varT sl)
		`appT` varT sl

{-
class SourceList c where
	data ListPos c
	listToken :: [c] -> Maybe (c, [c])
	listUpdatePos :: c -> ListPos c -> ListPos c
	listShowPos :: ListPos c -> String
-}

classSL th = classD (cxt []) sourceList [PlainTV c] [] [
	familyNoKindD dataFam listPosN [PlainTV c],
	sigD listTokenN $ arrowT `appT` (listT `appT` varT c) `appT`
		(conT (maybeN th) `appT` tupleBody),
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
	type Pos [c] = ListPos c
	getToken = listToken
	updatePos = listUpdatePos
	showPos = listShowPos
-}

instanceSrcStr _ =
	instanceD (cxt [classP sourceList [varT c]]) (conT source `appT` listC) [
		tySynInstD tokenN [listC] $ varT c,
		tySynInstD posN [listC] $ conT listPosN `appT` varT c,
		valD (varP getTokenN) (normalB $ varE listTokenN) [],
		valD (varP updatePosN) (normalB $ varE listUpdatePosN) [],
		valD (varP showPosN) (normalB $ varE listShowPosN) []
	 ]
	where
	c = mkName "c"
	listC = listT `appT` varT c

{-
instance SourceList Char where
	newtype ListPos Char = CharPos (Int, Int)
	listToken (c : s) = Just (c, s)
	listToken _ = Nothing
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
	 ) [],
	funD listTokenN [
		clause [infixP (varP c) (consN th) (varP s)]
			(normalB $ conE (justN th) `appE` tupleBody) [],
		clause [wildP] (normalB $ conE $ nothingN th) []
	 ],
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
