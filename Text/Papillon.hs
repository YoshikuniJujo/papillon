{-# LANGUAGE TemplateHaskell, PackageImports #-}

module Text.Papillon (
	papillon,
	StateT(..)
) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import "monads-tf" Control.Monad.State
import Control.Monad

import Text.Papillon.Parser

papillon :: QuasiQuoter
papillon = QuasiQuoter {
	quoteExp = undefined,
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = declaration
 }

declaration :: String -> DecsQ
declaration src = do
	let parsed@(name, typ, expr) = case dvDefinition $ parse src of
		Just (p, _) -> p
		_ -> error "bad"
--	runIO $ print $ (\(v, t, [([nl@(tv, Right p)], b)]) -> (v, t, tv)) parsed
--	some <- (\(v, t, [([(tv, Right p)], b)]) -> p) parsed
--	other <- (\(v, t, [([(tv, Right p)], b)]) -> b) parsed
--	runIO $ print some
--	runIO $ print other
	debug <- flip (valD $ varP $ mkName "debug") [] $ normalB $
		appE (varE $ mkName "putStrLn") (litE $ stringL "debug")
	r <- result
	pm <- pmonad
	d <- derivs
	pt <- parseT
	p <- funD (mkName "parse") [parseE]
	dvsm <- dvSomeM
	dvcm <- dvCharsM
	ps <- pSomes expr
	return [debug, r, pm, d, pt, p, dvsm, dvcm, ps]
	where
	c = clause [wildP] (normalB $ conE $ mkName "Nothing") []

derivs :: DecQ
derivs = flip (dataD (cxt []) (mkName "Derivs") []) [] $ [
	recC (mkName "Derivs") [
		varStrictType (mkName "dvSome") $ strictType notStrict $
			conT (mkName "Result") `appT` conT ''Char,
		varStrictType (mkName "dvChars") $ strictType notStrict $
			conT (mkName "Result") `appT` conT ''Char
	 ]
 ]

result :: DecQ
result = tySynD (mkName "Result") [PlainTV $ mkName "v"] $ conT ''Maybe `appT`
	(tupleT 2 `appT` varT (mkName "v") `appT` conT (mkName "Derivs"))
pmonad :: DecQ
pmonad = tySynD (mkName "PMonad") [] $ conT ''StateT `appT`
	conT (mkName "Derivs") `appT` conT ''Maybe

parseT :: DecQ
parseT = sigD (mkName "parse") $
	arrowT `appT` conT ''String `appT` conT (mkName "Derivs")
parseE :: ClauseQ
parseE = clause [varP $ mkName "s"] (normalB $ varE $ mkName "d") [
	flip (valD $ varP $ mkName "d") [] $ normalB $ (conE $ mkName "Derivs")
		`appE` (varE $ mkName "some")
		`appE` (varE $ mkName "chr"),
	flip (valD $ varP $ mkName "some") [] $ normalB $
		(varE $ mkName "runStateT") `appE` (varE $ mkName "pSome") `appE`
			(varE $ mkName "d"),
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

dvSomeM, dvCharsM :: DecQ
dvSomeM = flip (valD $ varP $ mkName "dvSomeM") [] $ normalB $
	conE 'StateT `appE` varE (mkName "dvSome")
dvCharsM = flip (valD $ varP $ mkName "dvCharsM") [] $ normalB $
	conE 'StateT `appE` varE (mkName "dvChars")

pSomes :: [([NameLeaf], ExpQ)] -> DecQ
pSomes sel = flip (valD $ varP $ mkName "pSome") [] $ normalB $
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
