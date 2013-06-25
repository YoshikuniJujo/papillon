{-# LANGUAGE TemplateHaskell, PackageImports, TypeFamilies, FlexibleContexts,
	FlexibleInstances #-}

module Text.Papillon (
	papillon,
	papillonStr,
	papillonStr',
	Source(..),
	SourceList(..),
	ListPos(..),
	list, list1,
	papOptional
) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Monad.Trans.Error (Error(..))

import Control.Applicative

import Text.Papillon.Parser hiding (initialPos)
import qualified Text.Papillon.Parser as P
import Data.IORef

import Text.Papillon.Class
import Text.Papillon.List

classSourceQ True
listDec True
optionalDec True

isOptionalUsed :: Peg -> Bool
isOptionalUsed = any isOptionalUsedDefinition

isOptionalUsedDefinition :: Definition -> Bool
isOptionalUsedDefinition (_, _, sel) = any isOptionalUsedSelection sel

isOptionalUsedSelection :: ExpressionHs -> Bool
isOptionalUsedSelection = any isOptionalUsedLeafName . fst

isOptionalUsedLeafName :: NameLeaf_ -> Bool
isOptionalUsedLeafName (Here nl) = isOptionalUsedLeafName' nl
isOptionalUsedLeafName (NotAfter nl) = isOptionalUsedLeafName' nl
isOptionalUsedLeafName (After nl) = isOptionalUsedLeafName' nl

isOptionalUsedLeafName' :: NameLeaf -> Bool
isOptionalUsedLeafName' (NameLeaf _ rf _) = isOptionalUsedReadFrom rf

isOptionalUsedReadFrom :: ReadFrom -> Bool
isOptionalUsedReadFrom (FromOptional _) = True
isOptionalUsedReadFrom (FromSelection sel) = any isOptionalUsedSelection sel
isOptionalUsedReadFrom _ = False

isListUsed :: Peg -> Bool
isListUsed = any isListUsedDefinition

isListUsedDefinition :: Definition -> Bool
isListUsedDefinition (_, _, sel) = any isListUsedSelection sel

isListUsedSelection :: ExpressionHs -> Bool
isListUsedSelection = any isListUsedLeafName . fst

isListUsedLeafName :: NameLeaf_ -> Bool
isListUsedLeafName (Here nl) = isListUsedLeafName' nl
isListUsedLeafName (NotAfter nl) = isListUsedLeafName' nl
isListUsedLeafName (After nl) = isListUsedLeafName' nl

isListUsedLeafName' :: NameLeaf -> Bool
isListUsedLeafName' (NameLeaf _ (FromList _) _) = True
isListUsedLeafName' (NameLeaf _ (FromList1 _) _) = True
isListUsedLeafName' _ = False

usingNames :: Peg -> [String]
usingNames = concatMap getNamesFromDefinition

getNamesFromDefinition :: Definition -> [String]
getNamesFromDefinition (_, _, sel) =
	concatMap getNamesFromExpressionHs sel

getNamesFromExpressionHs :: ExpressionHs -> [String]
getNamesFromExpressionHs = concatMap getLeafName . fst

getLeafName :: NameLeaf_ -> [String]
getLeafName (Here nl) = getLeafName' nl
getLeafName (NotAfter nl) = getLeafName' nl
getLeafName (After nl) = getLeafName' nl

getLeafName' :: NameLeaf -> [String]
getLeafName' (NameLeaf _ rf _) = getNamesFromReadFrom rf

getNamesFromReadFrom :: ReadFrom -> [String]
getNamesFromReadFrom (FromVariable n) = [n]
getNamesFromReadFrom (FromList rf) = getNamesFromReadFrom rf
getNamesFromReadFrom (FromList1 rf) = getNamesFromReadFrom rf
getNamesFromReadFrom (FromOptional rf) = getNamesFromReadFrom rf
getNamesFromReadFrom (FromSelection sel) = concatMap getNamesFromExpressionHs sel
getNamesFromReadFrom _ = []

catchErrorN, unlessN, errorN :: Bool -> Name
catchErrorN True = 'catchError
catchErrorN False = mkName "catchError"
unlessN True = 'unless
unlessN False = mkName "unless"
errorN True = ''Error
errorN False = mkName "Error"

{-
flipMaybe :: String -> PackratM a -> PackratM ()
flipMaybe errMsg action = do
	err <- (action >> return False) `catchError` const (return True)
	unless err $ throwErrorPackratM errMsg "not error"
-}

flipMaybeQ :: IORef Int -> Bool -> DecsQ
flipMaybeQ _ th = sequence [
	sigD (mkName "flipMaybe") $ forallT [PlainTV $ mkName "a"] (cxt []) $
		arrowT `appT` (conT $ mkName "String") `appT` (arrowT
		`appT` (conT (mkName "PackratM") `appT` varT (mkName "a"))
		`appT` (conT (mkName "PackratM") `appT` tupleT 0)), -- (mkName "()")),)
	funD (mkName "flipMaybe") $ (: []) $
		flip (clause [varP $ mkName "errMsg", varP $ mkName "act"]) [] $ normalB $ doE [
			bindS (varP $ mkName "err") $ infixApp
				actionReturnFalse
				(varE $ catchErrorN th)
				constReturnTrue,
			noBindS $ varE (unlessN th)
				`appE` varE (mkName "err")
				`appE` varThrowQ
					(infixApp
						(litE $ charL '!')
						(conE $ mkName ":")
						(varE $ mkName "errMsg"))
					"not match"
--					varE (mkName "undefined")
		 ]
 ]	where
	actionReturnFalse = infixApp
		(varE $ mkName "act")
		(varE $ mkName ">>")
		(varE (mkName "return") `appE` conE (mkName "False"))
	constReturnTrue = varE (mkName "const") `appE` 
		(varE (mkName "return") `appE` conE (mkName "True"))

-- varThrowQ :: ExpQ -> String -> ExpQ -> ExpQ
varThrowQ :: ExpQ -> String -> ExpQ
-- varThrowQ code msg tkn = varE (mkName "throwErrorPackratM")
varThrowQ code msg = varE (mkName "throwErrorPackratM")
	`appE` code
	`appE` litE (stringL msg)
--	`appE` tkn
	`appE` varE (mkName "undefined")

-- newThrowQ :: String -> String -> ExpQ -> ExpQ
newThrowQ :: String -> String -> ExpQ
-- newThrowQ code msg tkn = varE (mkName "throwErrorPackratM")
newThrowQ code msg = varE (mkName "throwErrorPackratM")
	`appE` litE (stringL code)
	`appE` litE (stringL msg)
--	`appE` tkn
	`appE` varE (mkName "undefined")

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
	let (ppp, pp, decsQ, atp, peg) = declaration' src
	decs <- runQ decsQ
	cls <- runQ $ classSourceQ False
	lst <- runQ $ listDec False
	opt <- runQ $ optionalDec False
	return $ ppp ++
		(if isListUsed peg || isOptionalUsed peg then "\nimport Control.Applicative\n" else "") ++
		pp ++ "\n" ++ show (ppr decs) ++ "\n" ++ atp ++
		"\n" ++ show (ppr cls) ++ "\n" ++
		(if isListUsed peg then show (ppr lst) else "") ++ "\n" ++
		if isOptionalUsed peg then show (ppr opt) else ""

returnN, stateTN, putN, stateTN', getN,
	strMsgN, throwErrorN, runStateTN, justN, mplusN,
	getTokenN, getsN :: Bool -> Name
returnN True = 'return
returnN False = mkName "return"
throwErrorN True = 'throwError
throwErrorN False = mkName "throwError"
strMsgN True = 'strMsg
strMsgN False = mkName "strMsg"
stateTN True = ''StateT
stateTN False = mkName "StateT"
putN True = 'put
putN False = mkName "put"
getsN True = 'gets
getsN False = mkName "gets"
stateTN' True = 'StateT
stateTN' False = mkName "StateT"
mplusN True = 'mplus
mplusN False = mkName "mplus"
getN True = 'get
getN False = mkName "get"
runStateTN True = 'runStateT
runStateTN False = mkName "runStateT"
justN True = 'Just
justN False = mkName "Just"
getTokenN True = 'getToken
getTokenN False = mkName "getToken"

eitherN :: Name
eitherN = mkName "Either"

declaration :: Bool -> String -> DecsQ
declaration th str = do
	let (src, tkn, parsed) = case dv_peg $ parse P.initialPos str of
		Right ((s, t, p), _) -> (s, t, p)
		_ -> error "bad"
	decParsed th src tkn parsed

declaration' :: String -> (String, String, DecsQ, String, Peg)
declaration' src = case dv_pegFile $ parse P.initialPos src of
	Right ((ppp, pp, (s, t, p), atp), _) ->
		(ppp, pp, decParsed False s t p, atp, p)
	Left err -> error $ "parse error: " ++ show err

decParsed :: Bool -> TypeQ -> TypeQ -> Peg -> DecsQ
decParsed th src tkn parsed = do
	glb <- runIO $ newIORef 0
	pet <- parseErrorT th
	tepm <- throwErrorPackratMQ th
	iepe <- instanceErrorParseError th
	r <- result src
	pm <- pmonad th src
	et <- errTypes tkn parsed
	d <- derivs th src tkn parsed
	pt <- parseT src th
	p <- funD (mkName "parse") [parseE th parsed]
	tdvm <- typeDvM parsed
	dvsm <- dvSomeM th parsed
	tdvcm <- typeDvCharsM th tkn
	dvcm <- dvCharsM th
	pts <- typeP parsed
	ps <- pSomes glb th parsed
	fm <- flipMaybeQ glb th
	return $ et : pet : tepm ++ [iepe] ++ fm ++ [pm, r, d, pt, p] ++ tdvm ++ dvsm ++
		[tdvcm, dvcm] ++ pts ++ ps

derivs :: Bool -> TypeQ -> TypeQ -> Peg -> DecQ
derivs _ src tkn peg = dataD (cxt []) (mkName "Derivs") [] [
	recC (mkName "Derivs") $ map derivs1 peg ++ [
		varStrictType (mkName "dvChars") $ strictType notStrict $
			conT (mkName "Result") `appT` tkn,
		varStrictType (mkName "dvPos") $ strictType notStrict $
			conT (mkName "Pos") `appT` src
	 ]
 ] []

errTypes :: TypeQ -> Peg -> DecQ
errTypes tkn peg = flip (dataD (cxt []) (mkName "ErrorTypes") []) [] $
	map (uncurry mkErrorType) $
		map (\(n, t, _) -> (n, t)) peg ++ [("Char", tkn)]

mkErrorType :: String -> TypeQ -> ConQ
mkErrorType name typ =
	normalC (mkName $ "ErrorType" ++ name) [strictType notStrict typ]

derivs1 :: Definition -> VarStrictTypeQ
derivs1 (name, typ, _) =
	varStrictType (mkName $ "dv_" ++ name) $ strictType notStrict $
		conT (mkName "Result") `appT` typ

{-

data ParseError pos
	= ParseError String String pos Derivs ExpQ
	deriving Show

-}

expQN :: Bool -> Name
expQN True = ''ExpQ
expQN False = mkName "ExpQ"

parseErrorT :: Bool -> DecQ
parseErrorT th = flip (dataD (cxt []) (mkName "ParseError") [PlainTV $ mkName "pos"])
	[] $ (:[]) $
	normalC (mkName "ParseError") [
		strictType notStrict $ conT $ mkName "String",
		strictType notStrict $ conT $ mkName "String",
		strictType notStrict $ varT $ mkName "pos",
		strictType notStrict $ conT $ mkName "Derivs",
		strictType notStrict $ conT $ expQN th
--		strictType notStrict $ conT $ mkName "ErrorTypes"
	 ]


{-

instance Pos s pos => Error (ParseError pos) where
	strMsg msg = ParseError "" msg initialPos

-}

instanceErrorParseError :: Bool -> DecQ
instanceErrorParseError th = instanceD
--	(cxt [classP (mkName "Pos") [varT $ mkName "s", varT $ mkName "pos"]])
	(cxt [	classP (mkName "Source") [varT $ mkName "s"],
		equalP (conT (mkName "Pos") `appT` varT (mkName "s"))
			(varT $ mkName "pos")])
	(conT (errorN th) `appT`
		(conT (mkName "ParseError") `appT` varT (mkName "pos")))
	[funD (strMsgN th) $ (: []) $ flip (clause [varP msg]) [] $ normalB ret]
	where
	msg = mkName "msg"
	ret = conE (mkName "ParseError")
		`appE` litE (stringL "")
		`appE` varE msg
		`appE` varE (mkName "initialPos")
		`appE` varE (mkName "undefined")
		`appE` varE (mkName "undefined")

{-

throwErrorPackratM :: String -> String -> String -> PackratM a
throwErrorPackratM code msg name = do
	pos <- gets dvPos
	d <- get
	throwError (ParseError code msg pos d (varE $ mkName $ "dv_" ++ name))

-}

infixr `arrT`

arrT :: TypeQ -> TypeQ -> TypeQ
arrT x y = arrowT `appT` x `appT` y

throwErrorPackratMQ :: Bool -> DecsQ
throwErrorPackratMQ th = sequence [
	sigD (mkName "throwErrorPackratM") $
		forallT [PlainTV $ mkName "a"] (cxt []) $
			conT (mkName "String")
			`arrT`
			conT (mkName "String")
			`arrT`
			conT (mkName "String")
			`arrT`
			(conT (mkName "PackratM") `appT` varT (mkName "a")),
	funD (mkName "throwErrorPackratM") $ (: []) $
		flip (clause [varP $ mkName "code", varP $ mkName "msg",
						varP $ mkName "_"]) [] $ normalB $ doE [
			bindS (varP $ mkName "pos") $
				varE (getsN th) `appE` varE (mkName "dvPos"),
			bindS (varP $ mkName "d") $ varE (getN th),
			noBindS $ varE (throwErrorN th) `appE`
				(conE (mkName "ParseError")
					`appE` varE (mkName "code")
					`appE` varE (mkName "msg")
					`appE` varE (mkName "pos")
					`appE` varE (mkName "d")
					`appE` varE (mkName "undefined"))
		 ]
 ]	where

result :: TypeQ -> DecQ
result src = tySynD (mkName "Result") [PlainTV $ mkName "v"] $
	conT eitherN `appT` pe `appT`
		(tupleT 2 `appT` varT (mkName "v") `appT` conT (mkName "Derivs"))
	where
	pe = conT (mkName "ParseError") `appT` (conT (mkName "Pos") `appT` src)

pmonad :: Bool -> TypeQ -> DecQ
pmonad th src = tySynD (mkName "PackratM") [] $ conT (stateTN th) `appT`
	conT (mkName "Derivs") `appT`
		(conT eitherN `appT` pe)
	where
	pe = conT (mkName "ParseError") `appT` (conT (mkName "Pos") `appT` src)

parseT :: TypeQ -> Bool -> DecQ
parseT src _ = sigD (mkName "parse") $ arrowT
	`appT` (conT (mkName "Pos") `appT` src) 
	`appT` (arrowT
		`appT` src
		`appT` conT (mkName "Derivs"))
parseE :: Bool -> Peg -> ClauseQ
parseE th = parseE' th . map (\(n, _, _) -> n)
parseE' :: Bool -> [String] -> ClauseQ
parseE' th names = clause [varP pos, varP $ mkName "s"]
					(normalB $ varE $ mkName "d") $ [

	flip (valD $ varP $ mkName "d") [] $ normalB $ appsE $
		conE (mkName "Derivs") :
			map (varE . mkName) names
			++ [varE (mkName "char"), varE pos]] ++
	map (parseE1 th) names ++ [
	flip (valD $ varP $ mkName "char") [] $ normalB $
		varE (mkName "flip") `appE` varE (runStateTN th) `appE`
			varE (mkName "d") `appE` caseE (varE (getTokenN th) `appE`
							varE (mkName "s")) [
				match	(justN th `conP` [
						tupP [varP (mkName "c"),
						varP (mkName "s'")]])
					(normalB $ doE [
						noBindS $ varE (putN th)
							`appE`
							(varE (mkName "parse") `appE`
								newPos `appE` varE (mkName "s'")),
						noBindS $ varE (returnN th) `appE`
							varE (mkName "c")
					 ])
					[],
				match	wildP
					(normalB $ newThrowQ "" "eof")
--					(normalB $ newThrowQ "" "eof" $
--						varE (mkName "ErrorTypeChar") `appE`
--						varE (mkName "c"))
					[]
			 ]
 ]
	where
	newPos = varE (mkName "updatePos")
		`appE` varE (mkName "c")
		`appE` varE pos
	pos = mkName "pos___hoge"
parseE1 :: Bool -> String -> DecQ
parseE1 th name = flip (valD $ varP $ mkName name) [] $ normalB $
	varE (runStateTN th) `appE` varE (mkName $ "p_" ++ name)
		`appE` varE (mkName "d")

typeDvM :: Peg -> DecsQ
typeDvM peg = let
	used = usingNames peg in
	uncurry (zipWithM typeDvM1) $ unzip $ filter ((`elem` used) . fst)
		$ map (\(n, t, _) -> (n, t)) peg

typeDvM1 :: String -> TypeQ -> DecQ
typeDvM1 f t = sigD (mkName $ "dv_" ++ f ++ "M") $
	conT (mkName "PackratM") `appT` t

dvSomeM :: Bool -> Peg -> DecsQ
dvSomeM th peg = mapM (dvSomeM1 th) $
	filter ((`elem` usingNames peg) . (\(n, _, _) -> n)) peg

dvSomeM1 :: Bool -> Definition -> DecQ
dvSomeM1 th (name, _, _) =
	flip (valD $ varP $ mkName $ "dv_" ++ name ++ "M") [] $ normalB $
		conE (stateTN' th) `appE` varE (mkName $ "dv_" ++ name)

typeDvCharsM :: Bool -> TypeQ -> DecQ
typeDvCharsM _ tkn =
	sigD (mkName "dvCharsM") $ conT (mkName "PackratM") `appT` tkn
dvCharsM :: Bool -> DecQ
dvCharsM th = flip (valD $ varP $ mkName "dvCharsM") [] $ normalB $
	conE (stateTN' th) `appE` varE (mkName "dvChars")

typeP :: Peg -> DecsQ
typeP = uncurry (zipWithM typeP1) . unzip . map (\(n, t, _) -> (n, t))

typeP1 :: String -> TypeQ -> DecQ
typeP1 f t = sigD (mkName $ "p_" ++ f) $ conT (mkName "PackratM") `appT` t

pSomes :: IORef Int -> Bool -> Peg -> DecsQ
pSomes g th = mapM $ pSomes1 g th

pSomes1 :: IORef Int -> Bool -> Definition -> DecQ
pSomes1 g th (name, _, sel) = flip (valD $ varP $ mkName $ "p_" ++ name) [] $
	normalB $ pSomes1Sel g th sel

pSomes1Sel :: IORef Int -> Bool -> Selection -> ExpQ
pSomes1Sel g th sel = varE (mkName "foldl1") `appE` varE (mplusN th) `appE`
	listE (map (uncurry $ pSome_ g th) sel)

pSome_ :: IORef Int -> Bool -> [NameLeaf_] -> ExpQ -> ExpQ
pSome_ g th nls ret = fmap DoE $ do
	x <- mapM (transLeaf g th) nls
	r <- noBindS $ varE (returnN th) `appE` ret
	return $ concat x ++ [r]

-- afterCheck :: Bool -> String -> Name -> ExpQ -> StmtQ
-- afterCheck th name n p = do
afterCheck :: Bool -> ExpQ -> StmtQ
afterCheck th p = do
	pp <- p
	noBindS $ condE p
		(varE (returnN th) `appE` conE (mkName "()"))
--		(newThrowQ (show $ ppr pp) "not match")
		(newThrowQ (show $ ppr pp) "not match")

beforeMatch :: Bool -> Name -> PatQ -> Q [Stmt]
beforeMatch th t n = do
	nn <- n
	sequence [
		noBindS $ caseE (varE t) [
			flip (match $ varPToWild n) [] $ normalB $
				varE (returnN th) `appE` tupE [],
			flip (match wildP) [] $ normalB $
				newThrowQ (show $ ppr nn) "not match pattern"
		 ],
		letS [flip (valD n) [] $ normalB $ varE t],
		noBindS $ varE (returnN th) `appE` tupE []
	 ]

getNewName :: IORef Int -> String -> Q Name
getNewName g n = do
	gn <- runIO $ readIORef g
	runIO $ modifyIORef g succ
	newName $ n ++ show gn

{-

showSelection :: Selection -> Q String = mapM showExpression

showNameLeaf :: NameLeaf -> Q String
showNameLeaf (NameLeafList pat sel) =
	(\ps ss -> sho (ppr ps) ++ ":(" ++ selS ++ ")*")
		<$> pat <*> showSelection sel

-}

transReadFrom :: IORef Int -> Bool -> ReadFrom -> ExpQ
transReadFrom _ _ FromToken = varE $ mkName "dvCharsM"
transReadFrom _ _ (FromVariable var) = varE $ mkName $ "dv_" ++ var ++ "M"
transReadFrom g th (FromSelection sel) = pSomes1Sel g th sel
transReadFrom g th (FromList rf) = varE (mkName "list") `appE` transReadFrom g th rf
transReadFrom g th (FromList1 rf) = varE (mkName "list1") `appE` transReadFrom g th rf
transReadFrom g th (FromOptional rf) = varE (mkName "papOptional") `appE` transReadFrom g th rf

{-
getErrTypeName :: ReadFrom -> Name
getErrTypeName 
-}

transLeaf' :: IORef Int -> Bool -> NameLeaf -> Q [Stmt]
transLeaf' g th (NameLeaf n rf p) = do
	t <- getNewName g "xx"
	nn <- n
	case nn of
		WildP -> sequence [
			bindS wildP $ transReadFrom g th rf,
			afterCheck th p
		 ]
		VarP _ -> do
			s <- bindS (varP t) $ transReadFrom g th rf
			m <- letS [flip (valD n) [] $ normalB $ varE t]
			c <- afterCheck th p
			return $ s : m : [c]
		_ -> do	s <- bindS (varP t) $ transReadFrom g th rf
			m <- beforeMatch th t n
			c <- afterCheck th p
			return $ s : m ++ [c]

transLeaf :: IORef Int -> Bool -> NameLeaf_ -> Q [Stmt]
transLeaf g th (Here nl) = transLeaf' g th nl
transLeaf g th (After nl) = do
	d <- getNewName g "ddd"
	sequence [
		bindS (varP d) $ varE (getN th),
		noBindS $ DoE <$> transLeaf' g th nl,
		noBindS $ varE (putN th) `appE` varE d]
transLeaf g th (NotAfter nl) = do
	d <- getNewName g "ddd"
	nls <- showNameLeaf nl
	sequence [
		bindS (varP d) $ varE (getN th),
		noBindS $ varE (mkName "flipMaybe")
			`appE` (litE $ stringL nls)
			`appE` (DoE <$> transLeaf' g th nl),
		noBindS $ varE (putN th) `appE` varE d]

varPToWild :: PatQ -> PatQ
varPToWild p = do
	pp <- p
	return $ vpw pp
	where
	vpw (VarP _) = WildP
	vpw (ConP n ps) = ConP n $ map vpw ps
	vpw (InfixP p1 n p2) = InfixP (vpw p1) n (vpw p2)
	vpw (UInfixP p1 n p2) = InfixP (vpw p1) n (vpw p2)
	vpw (ListP ps) = ListP $ vpw `map` ps
	vpw (TupP ps) = TupP $ vpw `map` ps
	vpw o = o
