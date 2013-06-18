{-# LANGUAGE TemplateHaskell, PackageImports #-}

module Text.Papillon (
	papillon,
	StateT(..)
) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import "monads-tf" Control.Monad.State

import Text.Papillon.Parser
-- import Parser

papillon :: QuasiQuoter
papillon = QuasiQuoter {
	quoteExp = undefined,
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = declaration
 }

declaration :: String -> DecsQ
declaration src = do
	let parsed = case dv_peg $ parse src of
		Just (p, _) -> p
		_ -> error "bad"
--	debug <- flip (valD $ varP $ mkName "debug") [] $ normalB $
--		appE (varE $ mkName "putStrLn") (litE $ stringL "debug")
	r <- result
	pm <- pmonad
	d <- derivs parsed
	pt <- parseT
	p <- funD (mkName "parse") [parseE parsed]
	tdvm <- typeDvM parsed
	dvsm <- dvSomeM parsed
	tdvcm <- typeDvCharsM
	dvcm <- dvCharsM
	pts <- typeP parsed
	ps <- pSomes parsed -- name expr
	return $ [pm, r, d, pt, p] ++ tdvm ++ dvsm ++ [tdvcm, dvcm] ++ pts ++ ps
	where
--	c = clause [wildP] (normalB $ conE $ mkName "Nothing") []

derivs :: Peg -> DecQ
derivs peg = flip (dataD (cxt []) (mkName "Derivs") []) [] $ [
	recC (mkName "Derivs") $ (map derivs1 peg) ++ [
		varStrictType (mkName "dvChars") $ strictType notStrict $
			conT (mkName "Result") `appT` conT ''Char
	 ]
 ]

derivs1 :: Definition -> VarStrictTypeQ
derivs1 (name, typ, _) =
	varStrictType (mkName $ "dv_" ++ name) $ strictType notStrict $
		conT (mkName "Result") `appT` conT typ

result :: DecQ
result = tySynD (mkName "Result") [PlainTV $ mkName "v"] $ conT ''Maybe `appT`
	(tupleT 2 `appT` varT (mkName "v") `appT` conT (mkName "Derivs"))

pmonad :: DecQ
pmonad = tySynD (mkName "PackratM") [] $ conT ''StateT `appT`
	conT (mkName "Derivs") `appT` conT ''Maybe

parseT :: DecQ
parseT = sigD (mkName "parse") $
	arrowT `appT` conT ''String `appT` conT (mkName "Derivs")
parseE :: Peg -> ClauseQ
parseE = parseE' . map (\(n, _, _) -> n)
parseE' :: [String] -> ClauseQ
parseE' names = clause [varP $ mkName "s"] (normalB $ varE $ mkName "d") $ [
	flip (valD $ varP $ mkName "d") [] $ normalB $ appsE $
		conE (mkName "Derivs") :
			map (varE . mkName) names
			++ [(varE $ mkName "char")]] ++
	map parseE1 names ++ [
	flip (valD $ varP $ mkName "char") [] $ normalB $
		(varE $ mkName "flip") `appE` (varE $ mkName "runStateT") `appE`
			(varE $ mkName "d") `appE` (doE [
				bindS	(infixP (varP $ mkName "c") (mkName ":")
						(varP $ mkName "s'")) $
					(varE 'return) `appE`
						(varE $ mkName "s"),
				noBindS $ (varE 'put) `appE`
					(varE (mkName "parse") `appE`
						varE (mkName "s'")),
				noBindS $ (varE 'return) `appE` varE (mkName "c")
			 ])
 ]
parseE1 :: String -> DecQ
parseE1 name = flip (valD $ varP $ mkName name) [] $ normalB $
	(varE $ mkName "runStateT") `appE` (varE $ mkName $ "p_" ++ name)
		`appE` (varE $ mkName "d")

typeDvM :: Peg -> DecsQ
typeDvM = uncurry (zipWithM typeDvM1) . unzip . map (\(n, t, _) -> (n, t))

typeDvM1 :: String -> Name -> DecQ
typeDvM1 f t = sigD (mkName $ "dv_" ++ f ++ "M") $ conT (mkName "PackratM") `appT` conT t

dvSomeM :: Peg -> DecsQ
dvSomeM peg = mapM dvSomeM1 peg

dvSomeM1 :: Definition -> DecQ
dvSomeM1 (name, _, _) = flip (valD $ varP $ mkName $ "dv_" ++ name ++ "M") [] $ normalB $
	conE 'StateT `appE` varE (mkName $ "dv_" ++ name)

typeDvCharsM :: DecQ
typeDvCharsM = sigD (mkName $ "dvCharsM") $ conT (mkName "PackratM") `appT` conT ''Char
dvCharsM :: DecQ
dvCharsM = flip (valD $ varP $ mkName "dvCharsM") [] $ normalB $
	conE 'StateT `appE` varE (mkName "dvChars")

typeP :: Peg -> DecsQ
typeP = uncurry (zipWithM typeP1) . unzip . map (\(n, t, _) -> (n, t))

typeP1 :: String -> Name -> DecQ
typeP1 f t = sigD (mkName $ "p_" ++ f) $ conT (mkName "PackratM") `appT` conT t

pSomes :: Peg -> DecsQ
pSomes = mapM pSomes1

pSomes1 :: Definition -> DecQ
pSomes1 (name, _, sel) = flip (valD $ varP $ mkName $ "p_" ++ name) [] $ normalB $
	varE 'msum `appE` listE (map (uncurry pSome_) sel)

pSome_ :: [NameLeaf] -> ExpQ -> ExpQ
pSome_ nls ret = doE $
	concatMap transLeaf nls ++ [noBindS $ (varE 'return) `appE` ret]

transLeaf :: NameLeaf -> [StmtQ]
transLeaf (n, Right p) = [
	bindS (varP n) $ varE $ mkName "dvCharsM",
	noBindS $ condE (p `appE` varE n)
		(varE 'return `appE` conE (mkName "()"))
		(varE 'fail `appE` litE (stringL "not match"))]
transLeaf (n, Left v) = [
	bindS (varP n) $ varE $ mkName $ "dv_" ++ v ++ "M"]
