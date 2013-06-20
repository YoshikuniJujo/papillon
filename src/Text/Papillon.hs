{-# LANGUAGE TemplateHaskell, PackageImports, TypeFamilies, FlexibleContexts #-}

module Text.Papillon (
	papillon,
	papillonStr,
	papillonStr',
	classSourceQ,
	Source(..),
	SourceList(..)
) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Monad.Trans.Error (Error(..))
import Data.Maybe

import Control.Applicative

import Text.Papillon.Parser
import Data.IORef

import Text.Papillon.Class

classSourceQ True

usingNames :: Peg -> [String]
usingNames = concatMap getNamesFromDefinition

getNamesFromDefinition :: Definition -> [String]
getNamesFromDefinition (_, _, sel) =
	concatMap getNamesFromExpressionHs sel

getNamesFromExpressionHs :: ExpressionHs -> [String]
getNamesFromExpressionHs = mapMaybe getLeafName . fst

getLeafName :: NameLeaf_ -> Maybe String
getLeafName (Here (_, Left n)) = Just n
getLeafName (NotAfter (_, Left n)) = Just n
getLeafName _ = Nothing

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
	cls <- runQ $ classSourceQ False
	return $ pp ++ "\n" ++ flipMaybeS ++ show (ppr decs) ++ "\n" ++ atp ++
		"\n" ++ show (ppr cls)

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

returnN, stateTN, stringN, putN, stateTN', msumN, getN,
	eitherN, strMsgN, throwErrorN, runStateTN, justN,
	getTokenN :: Bool -> Name
returnN True = 'return
returnN False = mkName "return"
throwErrorN True = 'throwError
throwErrorN False = mkName "throwError"
strMsgN True = 'strMsg
strMsgN False = mkName "strMsg"
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
runStateTN True = 'runStateT
runStateTN False = mkName "runStateT"
justN True = 'Just
justN False = mkName "Just"
getTokenN True = 'getToken
getTokenN False = mkName "getToken"

declaration :: Bool -> String -> DecsQ
declaration th str = do
--	fm <- dFlipMaybe
	let (src, tkn, parsed) = case dv_peg $ parse str of
		Right ((s, t, p), _) -> (s, t, p)
		_ -> error "bad"
	decParsed th src tkn parsed

declaration' :: String -> (String, DecsQ, String)
declaration' src = case dv_pegFile $ parse src of
	Right ((pp, (s, t, p), atp), _) ->
		(pp, decParsed False s t p, atp)
	_ -> error "bad"

decParsed :: Bool -> TypeQ -> TypeQ -> Peg -> DecsQ
decParsed th src tkn parsed = do
--	debug <- flip (valD $ varP $ mkName "debug") [] $ normalB $
--		appE (varE $ mkName "putStrLn") (litE $ stringL "debug")
	glb <- runIO $ newIORef 0
	r <- result th
	pm <- pmonad th
	d <- derivs th tkn parsed
	pt <- parseT src th
	p <- funD (mkName "parse") [parseE th parsed]
	tdvm <- typeDvM parsed
	dvsm <- dvSomeM th parsed
	tdvcm <- typeDvCharsM th tkn
	dvcm <- dvCharsM th
	pts <- typeP parsed
	ps <- pSomes glb th parsed -- name expr
	return $ {- fm ++ -} [pm, r, d, pt, p] ++ tdvm ++ dvsm ++ [tdvcm, dvcm] ++ pts ++ ps
	where
--	c = clause [wildP] (normalB $ conE $ mkName "Nothing") []

derivs :: Bool -> TypeQ -> Peg -> DecQ
derivs _ tkn peg = dataD (cxt []) (mkName "Derivs") [] [
	recC (mkName "Derivs") $ map derivs1 peg ++ [
		varStrictType (mkName "dvChars") $ strictType notStrict $
			conT (mkName "Result") `appT` tkn
	 ]
 ] []

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

parseT :: TypeQ -> Bool -> DecQ
parseT src _ = sigD (mkName "parse") $
	arrowT `appT` src `appT` conT (mkName "Derivs")
parseE :: Bool -> Peg -> ClauseQ
parseE th = parseE' th . map (\(n, _, _) -> n)
parseE' :: Bool -> [String] -> ClauseQ
parseE' th names = clause [varP $ mkName "s"] (normalB $ varE $ mkName "d") $ [
	flip (valD $ varP $ mkName "d") [] $ normalB $ appsE $
		conE (mkName "Derivs") :
			map (varE . mkName) names
			++ [varE (mkName "char")]] ++
	map (parseE1 th) names ++ [
	flip (valD $ varP $ mkName "char") [] $ normalB $
		varE (mkName "flip") `appE` varE (runStateTN th) `appE`
			varE (mkName "d") `appE` caseE (varE (getTokenN th) `appE`
								varE (mkName "s")) [
					match	(justN th `conP` [
							tupP [(varP (mkName "c")),
							(varP (mkName "s'"))]])
						(normalB $ doE [
							noBindS $ varE (putN th)
								`appE`
								(varE (mkName "parse") `appE` varE (mkName "s'")),
							noBindS $ varE (returnN th) `appE`
								varE (mkName "c")
						 ])
						[],
					match	wildP
						(normalB $ varE (throwErrorN th) `appE`
							(varE (strMsgN th) `appE`
							litE (stringL "eof")))
						[]
				 ]
 ]
parseE1 :: Bool -> String -> DecQ
parseE1 th name = flip (valD $ varP $ mkName name) [] $ normalB $
	varE (runStateTN th) `appE` varE (mkName $ "p_" ++ name)
		`appE` varE (mkName "d")

typeDvM :: Peg -> DecsQ
typeDvM peg = let
	used = usingNames peg in
	uncurry (zipWithM typeDvM1) $ unzip $ filter ((`elem` used) . fst)
		$ map (\(n, t, _) -> (n, t)) peg

typeDvM1 :: String -> Name -> DecQ
typeDvM1 f t = sigD (mkName $ "dv_" ++ f ++ "M") $ conT (mkName "PackratM") `appT` conT t

dvSomeM :: Bool -> Peg -> DecsQ
dvSomeM th peg = mapM (dvSomeM1 th) $
	filter ((`elem` usingNames peg) . (\(n, _, _) -> n)) peg

dvSomeM1 :: Bool -> Definition -> DecQ
dvSomeM1 th (name, _, _) = flip (valD $ varP $ mkName $ "dv_" ++ name ++ "M") [] $ normalB $
	conE (stateTN' th) `appE` varE (mkName $ "dv_" ++ name)

typeDvCharsM :: Bool -> TypeQ -> DecQ
typeDvCharsM _ tkn =
	sigD (mkName "dvCharsM") $ conT (mkName "PackratM") `appT` tkn
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

pSome_ :: IORef Int -> Bool -> [NameLeaf_] -> ExpQ -> ExpQ
pSome_ g th nls ret = fmap DoE $ do
	x <- mapM (transLeaf g th) nls
	r <- noBindS $ varE (returnN th) `appE` ret
	return $ concat x ++ [r]

transLeaf :: IORef Int -> Bool -> NameLeaf_ -> Q [Stmt]
transLeaf g th (Here (n, Right p)) = do
	gn <- runIO $ readIORef g
	runIO $ modifyIORef g succ
	t <- newName $ "xx" ++ show gn
	nn <- n
	case nn of
		VarP _ -> sequence [
			bindS (varP t) $ varE $ mkName "dvCharsM",
			noBindS $ condE (p `appE` varE t)
				(varE (returnN th) `appE` conE (mkName "()"))
				(varE (throwErrorN th) `appE`
					(varE (strMsgN th) `appE`
						litE (stringL "not match"))),
			noBindS $ caseE (varE t) [
				flip (match $ varPToWild n) [] $ normalB $
					varE (returnN th) `appE` tupE []
			 ],
			letS [flip (valD n) [] $ normalB $ varE t],
			noBindS $ varE (returnN th) `appE` tupE []
		 ]
		WildP -> sequence [
			bindS (varP t) $ varE $ mkName "dvCharsM",
			noBindS $ condE (p `appE` varE t)
				(varE (returnN th) `appE` conE (mkName "()"))
				(varE (throwErrorN th) `appE`
					(varE (strMsgN th) `appE`
						litE (stringL "not match"))),
			noBindS $ caseE (varE t) [
				flip (match $ varPToWild n) [] $ normalB $
					varE (returnN th) `appE` tupE []
			 ],
			letS [flip (valD n) [] $ normalB $ varE t],
			noBindS $ varE (returnN th) `appE` tupE []
		 ]
		_ -> sequence [
			bindS (varP t) $ varE $ mkName "dvCharsM",
			noBindS $ condE (p `appE` varE t)
				(varE (returnN th) `appE` conE (mkName "()"))
				(varE (throwErrorN th) `appE`
					(varE (strMsgN th) `appE`
						litE (stringL "not match"))),
			noBindS $ caseE (varE t) [
				flip (match $ varPToWild n) [] $ normalB $
					varE (returnN th) `appE` tupE [],
				flip (match wildP) [] $ normalB $ varE (throwErrorN th) `appE`
					(varE (strMsgN th) `appE` litE (stringL "not match"))
			 ],
			letS [flip (valD n) [] $ normalB $ varE t],
			noBindS $ varE (returnN th) `appE` tupE []
		 ]
transLeaf g th (Here (n, Left v)) = do
	nn <- n
	case nn of
		VarP _ -> sequence [
			bindS n $ varE $ mkName $ "dv_" ++ v ++ "M",
			noBindS $ varE (returnN th) `appE` conE (mkName "()")]
		WildP -> sequence [
			bindS wildP $ varE $ mkName $ "dv_" ++ v ++ "M",
			noBindS $ varE (returnN th) `appE` conE (mkName "()")]
		_ -> do	gn <- runIO $ readIORef g
			runIO $ modifyIORef g succ
			t <- newName $ "xx" ++ show gn
			sequence [
				bindS (varP t) $ varE $ mkName $ "dv_" ++ v ++ "M",
				noBindS $ caseE (varE t) [
					flip (match $ varPToWild n) [] $ normalB $
						varE (returnN th) `appE`
							tupE [],
					flip (match wildP) [] $ normalB $
						varE (throwErrorN th) `appE`
							(varE (strMsgN th) `appE`
								litE (stringL "not match"))
				 ],
				bindS n $ varE (returnN th) `appE` varE t
			 ]
transLeaf g th (NotAfter (n, Right p)) = do
	d <- newName "d"
	sequence [
		bindS (varP d) $ varE (getN th),
		noBindS $ varE (flipMaybeN th) `appE`
			(DoE <$> transLeaf g th (Here (n, Right p))),
		noBindS $ varE (putN th) `appE` varE d]
transLeaf g th (NotAfter (n, Left v)) = do
	d <- newName "d"
	sequence [
		bindS (varP d) $ varE (getN th),
		noBindS $ varE (flipMaybeN th) `appE`
			(DoE <$> transLeaf g th (Here (n, Left v))),
{-
		noBindS $ varE (flipMaybeN th) `appE`
			varE (mkName $ "dv_" ++ v ++ "M"),
-}
		noBindS $ varE (putN th) `appE` varE d]

varPToWild :: PatQ -> PatQ
varPToWild p = do
	pp <- p
	return $ vpw pp
	where
	vpw (VarP _) = WildP
	vpw (ConP n ps) = ConP n $ map vpw ps
	vpw o = o
