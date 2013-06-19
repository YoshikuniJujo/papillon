{-# LANGUAGE TemplateHaskell, PackageImports #-}

module Text.Papillon (
	papillon,
	papillonStr,
	papillonStr',
	StateT(..),
) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import "monads-tf" Control.Monad.State

import Control.Applicative

import Text.Papillon.Parser

flipMaybe :: StateT s Maybe a -> StateT s Maybe ()
flipMaybe action = StateT $ \s -> case runStateT action s of
	Nothing -> Just ((), s)
	_ -> Nothing

papillon :: QuasiQuoter
papillon = QuasiQuoter {
	quoteExp = undefined,
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = declaration True
 }

papillonStr :: String -> IO String
papillonStr src = show . ppr <$> runQ (declaration False src)

papillonStr' :: String -> IO String
papillonStr' src = do
	let (pp, decsQ, atp) = declaration' src
	decs <- runQ decsQ
	return $ pp ++ "\n" ++ flipMaybeS ++ show (ppr decs) ++ "\n" ++ atp

flipMaybeS :: String
flipMaybeS =
	"flipMaybe :: StateT s Maybe a -> StateT s Maybe ()\n" ++
	"flipMaybe action = StateT $ \\s -> case runStateT action s of\n" ++
	"\tNothing -> Just ((), s)\n" ++
	"\t_ -> Nothing\n\n"

flipMaybeN :: Bool -> Name
flipMaybeN True = 'flipMaybe
flipMaybeN False = mkName "flipMaybe"

returnN, failN, charN, maybeN, stateTN, stringN, putN, stateTN', msumN, getN ::
	Bool -> Name
returnN True = 'return
returnN False = mkName "return"
failN True = 'fail
failN False = mkName "fail"
charN True = ''Char
charN False = mkName "Char"
maybeN True = ''Maybe
maybeN False = mkName "Maybe"
stateTN True = ''StateT
stateTN False = mkName "StateT"
stringN True = ''String
stringN False = mkName "String"
putN True = 'put
putN False = mkName "put"
stateTN' True = 'StateT
stateTN' False = mkName "StateT"
msumN True = 'msum
msumN False = mkName "msum"
getN True = 'get
getN False = mkName "get"

declaration :: Bool -> String -> DecsQ
declaration th src = do
--	fm <- dFlipMaybe
	let parsed = case dv_peg $ parse src of
		Just (p, _) -> p
		_ -> error "bad"
	decParsed th parsed

declaration' :: String -> (String, DecsQ, String)
declaration' src = case dv_pegFile $ parse src of
	Just ((pp, p, atp), _) -> (pp, decParsed False p, atp)
	_ -> error "bad"

decParsed :: Bool -> Peg -> DecsQ
decParsed th parsed = do
--	debug <- flip (valD $ varP $ mkName "debug") [] $ normalB $
--		appE (varE $ mkName "putStrLn") (litE $ stringL "debug")
	r <- result th
	pm <- pmonad th
	d <- derivs th parsed
	pt <- parseT th
	p <- funD (mkName "parse") [parseE th parsed]
	tdvm <- typeDvM parsed
	dvsm <- dvSomeM th parsed
	tdvcm <- typeDvCharsM th
	dvcm <- dvCharsM th
	pts <- typeP parsed
	ps <- pSomes th parsed -- name expr
	return $ {- fm ++ -} [pm, r, d, pt, p] ++ tdvm ++ dvsm ++ [tdvcm, dvcm] ++ pts ++ ps
	where
--	c = clause [wildP] (normalB $ conE $ mkName "Nothing") []

derivs :: Bool -> Peg -> DecQ
derivs th peg = flip (dataD (cxt []) (mkName "Derivs") []) [] $ [
	recC (mkName "Derivs") $ (map derivs1 peg) ++ [
		varStrictType (mkName "dvChars") $ strictType notStrict $
			conT (mkName "Result") `appT` conT (charN th)
	 ]
 ]

derivs1 :: Definition -> VarStrictTypeQ
derivs1 (name, typ, _) =
	varStrictType (mkName $ "dv_" ++ name) $ strictType notStrict $
		conT (mkName "Result") `appT` conT typ

result :: Bool -> DecQ
result th = tySynD (mkName "Result") [PlainTV $ mkName "v"] $ conT (maybeN th) `appT`
	(tupleT 2 `appT` varT (mkName "v") `appT` conT (mkName "Derivs"))

pmonad :: Bool -> DecQ
pmonad th = tySynD (mkName "PackratM") [] $ conT (stateTN th) `appT`
	conT (mkName "Derivs") `appT` conT (maybeN th)

parseT :: Bool -> DecQ
parseT th = sigD (mkName "parse") $
	arrowT `appT` conT (stringN th) `appT` conT (mkName "Derivs")
parseE :: Bool -> Peg -> ClauseQ
parseE th = parseE' th . map (\(n, _, _) -> n)
parseE' :: Bool -> [String] -> ClauseQ
parseE' th names = clause [varP $ mkName "s"] (normalB $ varE $ mkName "d") $ [
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
					(varE $ returnN th) `appE`
						(varE $ mkName "s"),
				noBindS $ (varE $ putN th) `appE`
					(varE (mkName "parse") `appE`
						varE (mkName "s'")),
				noBindS $ (varE $ returnN th) `appE` varE (mkName "c")
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

dvSomeM :: Bool -> Peg -> DecsQ
dvSomeM th peg = mapM (dvSomeM1 th) peg

dvSomeM1 :: Bool -> Definition -> DecQ
dvSomeM1 th (name, _, _) = flip (valD $ varP $ mkName $ "dv_" ++ name ++ "M") [] $ normalB $
	conE (stateTN' th) `appE` varE (mkName $ "dv_" ++ name)

typeDvCharsM :: Bool -> DecQ
typeDvCharsM th =
	sigD (mkName $ "dvCharsM") $ conT (mkName "PackratM") `appT` conT (charN th)
dvCharsM :: Bool -> DecQ
dvCharsM th = flip (valD $ varP $ mkName "dvCharsM") [] $ normalB $
	conE (stateTN' th) `appE` varE (mkName "dvChars")

typeP :: Peg -> DecsQ
typeP = uncurry (zipWithM typeP1) . unzip . map (\(n, t, _) -> (n, t))

typeP1 :: String -> Name -> DecQ
typeP1 f t = sigD (mkName $ "p_" ++ f) $ conT (mkName "PackratM") `appT` conT t

pSomes :: Bool -> Peg -> DecsQ
pSomes th = mapM $ pSomes1 th

pSomes1 :: Bool -> Definition -> DecQ
pSomes1 th (name, _, sel) = flip (valD $ varP $ mkName $ "p_" ++ name) [] $ normalB $
	varE (msumN th) `appE` listE (map (uncurry $ pSome_ th) sel)

pSome_ :: Bool -> [NameLeaf] -> ExpQ -> ExpQ
pSome_ th nls ret = doE $
	concatMap (transLeaf th) nls ++ [noBindS $ (varE $ returnN th) `appE` ret]

transLeaf :: Bool -> NameLeaf -> [StmtQ]
transLeaf th (n, (Here (Right p))) = [
	bindS (varP n) $ varE $ mkName "dvCharsM",
	noBindS $ condE (p `appE` varE n)
		(varE (returnN th) `appE` conE (mkName "()"))
		(varE (failN th) `appE` litE (stringL "not match"))]
transLeaf _ (n, (Here (Left v))) = [
	bindS (varP n) $ varE $ mkName $ "dv_" ++ v ++ "M"]
transLeaf th (n, (NotAfter (Right p))) = [
	bindS (varP $ mkName "d") $ varE (getN th),
	bindS (varP n) $ varE $ mkName "dvCharsM",
	noBindS $ condE (p `appE` varE n)
		(varE (failN th) `appE` litE (stringL "not match"))
		(varE (returnN th) `appE` conE (mkName "()")),
	noBindS $ varE (putN th) `appE` (varE $ mkName "d")]
transLeaf th (_, (NotAfter (Left v))) = [
	bindS (varP $ mkName "d") $ varE (getN th),
	noBindS $ varE (flipMaybeN th) `appE`
		varE (mkName $ "dv_" ++ v ++ "M"),
	noBindS $ varE (putN th) `appE` (varE $ mkName "d")]
