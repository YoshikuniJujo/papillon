{-# LANGUAGE TemplateHaskell, PackageImports #-}

module Text.Papillon (
	papillon,
	StateT(..)
) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import "monads-tf" Control.Monad.State
import Control.Monad

-- import Text.Papillon.Parser
import Parser

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
	debug <- flip (valD $ varP $ mkName "debug") [] $ normalB $
		appE (varE $ mkName "putStrLn") (litE $ stringL "debug")
	r <- result
	pm <- pmonad
	d <- derivs parsed
	pt <- parseT
	p <- funD (mkName "parse") [parseE parsed]
	dvsm <- dvSomeM parsed
	dvcm <- dvCharsM
	ps <- pSomes parsed -- name expr
	return $ [debug, r, pm, d, pt, p] ++ dvsm ++ [dvcm] ++ ps
	where
	c = clause [wildP] (normalB $ conE $ mkName "Nothing") []

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
pmonad = tySynD (mkName "PMonad") [] $ conT ''StateT `appT`
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
			++ [(varE $ mkName "chr")]] ++
	map parseE1 names ++ [
	flip (valD $ varP $ mkName "chr") [] $ normalB $
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

dvSomeM :: Peg -> DecsQ
dvSomeM peg = mapM dvSomeM1 peg

dvSomeM1 :: Definition -> DecQ
dvSomeM1 (name, _, _) = flip (valD $ varP $ mkName $ "dv_" ++ name ++ "M") [] $ normalB $
	conE 'StateT `appE` varE (mkName $ "dv_" ++ name)
dvCharsM :: DecQ
dvCharsM = flip (valD $ varP $ mkName "dvCharsM") [] $ normalB $
	conE 'StateT `appE` varE (mkName "dvChars")

pSomes :: Peg -> DecsQ
pSomes = mapM pSomes1

pSomes1 :: Definition -> DecQ
pSomes1 (name, _, sel) = flip (valD $ varP $ mkName $ "p_" ++ name) [] $ normalB $
	varE 'msum `appE` listE (map (uncurry pSome_) sel)

pSome :: [NameLeaf] -> ExpQ -> DecQ
pSome nls ret = flip (valD $ varP $ mkName "pSome") [] $ normalB $ pSome_ nls ret

pSome_ :: [NameLeaf] -> ExpQ -> ExpQ
pSome_ nls ret = doE $
	concatMap transLeaf nls ++ [noBindS $ (varE 'return) `appE` ret]

transLeaf :: NameLeaf -> [StmtQ]
transLeaf (n, Right p) = [
	bindS (varP n) $ varE $ mkName "dvCharsM",
	noBindS $ condE (p `appE` varE n)
		(varE 'return `appE` varE n)
		(varE 'fail `appE` litE (stringL "not match"))]
transLeaf (n, Left v) = [
	bindS (varP n) $ varE $ mkName $ "dv_" ++ v ++ "M"]
