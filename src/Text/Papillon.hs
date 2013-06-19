{-# LANGUAGE TemplateHaskell, PackageImports, TypeFamilies, FlexibleContexts #-}

module Text.Papillon (
	papillon,
	papillonStr,
	papillonStr'
) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Monad.Trans.Error (Error(..))

import Control.Applicative

import Text.Papillon.Parser
import Data.IORef

flipMaybe :: (Error (ErrorType me), MonadError me) =>
	StateT s me a -> StateT s me ()
flipMaybe action = do
	err <- (action >> return False) `catchError` const (return True)
	unless err $ throwError $ strMsg "not error"

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
{-
	"instance MonadError Maybe where\n" ++
	"\ttype ErrorType Maybe = ()\n" ++
	"\tthrowError () = Nothing\n" ++
	"\tcatchError action recover = recover ()\n\n" ++
-}

	"flipMaybe :: (Error (ErrorType me), MonadError me) =>\n" ++
	"\tStateT s me a -> StateT s me ()\n" ++
	"flipMaybe action = do\n" ++
	"\terr <- (action >> return False) `catchError` const (return True)\n" ++
	"\tunless err $ throwError $ strMsg \"not error\"\n"

flipMaybeN :: Bool -> Name
flipMaybeN True = 'flipMaybe
flipMaybeN False = mkName "flipMaybe"

returnN, charN, stateTN, stringN, putN, stateTN', msumN, getN,
	eitherN, whenN, nullN, strMsgN, throwErrorN, runStateTN :: Bool -> Name
returnN True = 'return
returnN False = mkName "return"
throwErrorN True = 'throwError
throwErrorN False = mkName "throwError"
strMsgN True = 'strMsg
strMsgN False = mkName "strMsg"
charN True = ''Char
charN False = mkName "Char"
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
eitherN True = ''Either
eitherN False = mkName "Either"
whenN True = 'when
whenN False = mkName "when"
nullN True = 'null
nullN False = mkName "null"
runStateTN True = 'runStateT
runStateTN False = mkName "runStateT"

declaration :: Bool -> String -> DecsQ
declaration th src = do
--	fm <- dFlipMaybe
	let parsed = case dv_peg $ parse src of
		Right (p, _) -> p
		_ -> error "bad"
	decParsed th parsed

declaration' :: String -> (String, DecsQ, String)
declaration' src = case dv_pegFile $ parse src of
	Right ((pp, p, atp), _) -> (pp, decParsed False p, atp)
	_ -> error "bad"

decParsed :: Bool -> Peg -> DecsQ
decParsed th parsed = do
--	debug <- flip (valD $ varP $ mkName "debug") [] $ normalB $
--		appE (varE $ mkName "putStrLn") (litE $ stringL "debug")
	glb <- runIO $ newIORef 0
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
	ps <- pSomes glb th parsed -- name expr
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
result th = tySynD (mkName "Result") [PlainTV $ mkName "v"] $
	conT (eitherN th) `appT` conT (stringN th) `appT`
		(tupleT 2 `appT` varT (mkName "v") `appT` conT (mkName "Derivs"))

pmonad :: Bool -> DecQ
pmonad th = tySynD (mkName "PackratM") [] $ conT (stateTN th) `appT`
	conT (mkName "Derivs") `appT`
		(conT (eitherN th) `appT` conT (stringN th))

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
	map (parseE1 th) names ++ [
	flip (valD $ varP $ mkName "char") [] $ normalB $
		(varE $ mkName "flip") `appE` (varE $ runStateTN th) `appE`
			(varE $ mkName "d") `appE` (doE [
				noBindS $ varE (whenN th) `appE`
					(varE (nullN th) `appE` varE (mkName "s"))
					`appE`
					(varE (throwErrorN th) `appE`
						(varE (strMsgN th) `appE`
							litE (stringL "eof"))),
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
parseE1 :: Bool -> String -> DecQ
parseE1 th name = flip (valD $ varP $ mkName name) [] $ normalB $
	(varE $ runStateTN th) `appE` (varE $ mkName $ "p_" ++ name)
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

pSomes :: IORef Int -> Bool -> Peg -> DecsQ
pSomes g th = mapM $ pSomes1 g th

pSomes1 :: IORef Int -> Bool -> Definition -> DecQ
pSomes1 g th (name, _, sel) = flip (valD $ varP $ mkName $ "p_" ++ name) [] $ normalB $
	varE (msumN th) `appE` listE (map (uncurry $ pSome_ g th) sel)

pSome_ :: IORef Int -> Bool -> [NameLeaf] -> ExpQ -> ExpQ
pSome_ g th nls ret = fmap DoE $ do
	x <- (mapM (transLeaf g th) nls)
	r <- noBindS $ (varE $ returnN th) `appE` ret
	return $ concat x ++ [r]

transLeaf :: IORef Int -> Bool -> NameLeaf -> Q [Stmt]
transLeaf g th (n, (Here (Right p))) = do
	gn <- runIO $ readIORef g
	runIO $ modifyIORef g succ
	t <- newName $ "xx" ++ show gn
	sequence [
		bindS (varP t) $ varE $ mkName "dvCharsM",
		letS [flip (valD n) [] $ normalB $ varE t],
		noBindS $ condE (p `appE` varE t)
			(varE (returnN th) `appE` conE (mkName "()"))
			(varE (throwErrorN th) `appE`
				(varE (strMsgN th) `appE` litE (stringL "not match")))]
transLeaf g th (n, (Here (Left v))) = do
	nn <- n
	case nn of
		VarP _ -> sequence [
			bindS n $ varE $ mkName $ "dv_" ++ v ++ "M"]
		_ -> do	gn <- runIO $ readIORef g
			runIO $ modifyIORef g succ
			t <- newName $ "xx" ++ show gn
			sequence [
				bindS (varP t) $ varE $ mkName $ "dv_" ++ v ++ "M",
				noBindS $ caseE (varE t) [
					flip (match $ varPToWild n) [] $ normalB $ varE (returnN th) `appE`
						(tupE []),
					flip (match wildP) [] $ normalB $ varE (throwErrorN th) `appE`
						(varE (strMsgN th) `appE`
							litE (stringL "not match"))
				 ],
				bindS n $ varE (returnN th) `appE` varE t
			 ]
transLeaf g th (n, (NotAfter (Right p))) = sequence [
	bindS (varP $ mkName "d") $ varE (getN th),
	noBindS $ varE (flipMaybeN th) `appE`
		(DoE <$> (transLeaf g th (n, (Here (Right p))))),
	noBindS $ varE (putN th) `appE` (varE $ mkName "d")]
transLeaf _ th (_, (NotAfter (Left v))) = sequence [
	bindS (varP $ mkName "d") $ varE (getN th),
	noBindS $ varE (flipMaybeN th) `appE`
		varE (mkName $ "dv_" ++ v ++ "M"),
	noBindS $ varE (putN th) `appE` (varE $ mkName "d")]

varPToWild :: PatQ -> PatQ
varPToWild p = do
	pp <- p
	return $ vpw pp
	where
	vpw (VarP _) = WildP
	vpw (ConP n ps) = ConP n $ map vpw ps
	vpw o = o
