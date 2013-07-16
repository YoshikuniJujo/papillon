{-# LANGUAGE TemplateHaskell, PackageImports, TypeFamilies, FlexibleContexts,
	FlexibleInstances #-}

module Text.Papillon (
	papillon,
	papillonStr,
	papillonConstant,
	ParseError(..)
) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error

import Control.Applicative

import Text.Papillon.Parser
import Data.IORef
import Data.List

import Text.Papillon.Class
import Text.Papillon.List

isOptionalUsed :: Peg -> Bool
isOptionalUsed = any isOptionalUsedDefinition

isOptionalUsedDefinition :: Definition -> Bool
isOptionalUsedDefinition (_, _, sel) = any isOptionalUsedSelection sel

isOptionalUsedSelection :: ExpressionHs -> Bool
isOptionalUsedSelection = any isOptionalUsedLeafName . fst

isOptionalUsedLeafName :: NameLeaf_ -> Bool
isOptionalUsedLeafName (Here nl) = isOptionalUsedLeafName' nl
isOptionalUsedLeafName (NotAfter nl _) = isOptionalUsedLeafName' nl
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
isListUsedLeafName (NotAfter nl _) = isListUsedLeafName' nl
isListUsedLeafName (After nl) = isListUsedLeafName' nl

isListUsedLeafName' :: NameLeaf -> Bool
isListUsedLeafName' (NameLeaf _ (FromList _) _) = True
isListUsedLeafName' (NameLeaf _ (FromList1 _) _) = True
isListUsedLeafName' _ = False

catchErrorN, unlessN :: Bool -> Name
catchErrorN True = 'catchError
catchErrorN False = mkName "catchError"
unlessN True = 'unless
unlessN False = mkName "unless"

smartDoE :: [Stmt] -> Exp
smartDoE [NoBindS ex] = ex
smartDoE stmts = DoE stmts

flipMaybeBody :: Bool -> ExpQ -> ExpQ -> ExpQ -> ExpQ -> ExpQ -> ExpQ
flipMaybeBody th code com d ns act = doE [
	bindS (varP $ mkName "err") $ infixApp
		actionReturnFalse
		(varE $ catchErrorN th)
		constReturnTrue,
	noBindS $ varE (unlessN th)
		`appE` varE (mkName "err")
		`appE` throwErrorPackratMBody th
			(infixApp (litE $ charL '!') (conE $ mkName ":") code)
			(stringE "not match: ") com d ns
 ]	where
	actionReturnFalse = infixApp act (varE $ mkName ">>")
		(varE (mkName "return") `appE` conE (mkName "False"))
	constReturnTrue = varE (mkName "const") `appE` 
		(varE (mkName "return") `appE` conE (mkName "True"))

newThrowQ :: Bool -> String -> String -> Name -> [String] -> String -> ExpQ
newThrowQ th code msg d ns com =
	throwErrorPackratMBody th (stringE code) (stringE msg) (stringE com)
		(varE d) (listE $ map stringE ns)

papillon :: QuasiQuoter
papillon = QuasiQuoter {
	quoteExp = undefined,
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = declaration True
 }

papillonStr :: String -> IO ([String], String, String)
papillonStr src = do
	let (mn, ppp, pp, decsQ, atp, pegg) = declaration' src
	decs <- runQ decsQ
	return $ (
		mn,
		ppp,
		(if isListUsed pegg || isOptionalUsed pegg
			then "\nimport Control.Applicative\n" else "") ++
		pp ++ "\n" ++ show (ppr decs) ++ "\n" ++ atp ++ "\n")

papillonConstant :: IO String
papillonConstant = do
	pe <- runQ $ parseErrorT False
	iepe <- runQ $ instanceErrorParseError False
	return $ show (ppr [pe, iepe]) ++ "\n"

returnN, putN, stateTN', getN,
	throwErrorN, runStateTN, justN, mplusN,
	getsN :: Bool -> Name
returnN True = 'return
returnN False = mkName "return"
throwErrorN True = 'throwError
throwErrorN False = mkName "throwError"
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

eitherN :: Name
eitherN = mkName "Either"

declaration :: Bool -> String -> DecsQ
declaration th str = do
	let (src, tkn, parsed) = case peg $ parse str of
		Right ((s, t, p), _) -> (s, t, p)
		Left err -> error $ "parse error: " ++ showParseError err
	decs <- decParsed th src tkn parsed
	pepst <- pePositionST
	pepsd <- pePositionSD
	return $ pepst : pepsd : decs

declaration' :: String -> ([String], String, String, DecsQ, String, Peg)
declaration' src = case pegFile $ parse src of
	Right ((mn, ppp, pp, (s, t, p), atp), _) ->
		(mn, ppp, pp, addPePositionS s t p, atp, p)
	Left err -> error $ "parse error: " ++ showParseError err
	where
	addPePositionS s t p = if "pePositionS" `isInfixOf` src
		then (\pt pd ds -> pt : pd : ds)
			<$> pePositionST
			<*> pePositionSD
			<*> decParsed False s t p
		else decParsed False s t p

showParseError :: ParseError (Pos String) Derivs -> String
showParseError (ParseError c m _ d ns (ListPos (CharPos p))) =
	unwords (map (showReading d) ns) ++ (if null ns then "" else " ") ++
	m ++ c ++ " at position: " ++ show p

showReading :: Derivs -> String -> String
showReading d "derivsChars" = case derivsChars d of
	Right (c, _) -> show c
	Left _ -> error "bad"
showReading _ n = "yet: " ++ n

decParsed :: Bool -> TypeQ -> TypeQ -> Peg -> DecsQ
decParsed th src tkn parsed = do
	glb <- runIO $ newIORef 0
	d <- derivs th src tkn parsed
	pt <- parseT src th
	p <- funD (mkName "parse") [parseEE glb th parsed]
	cls <- runQ $ classSourceQ False
	return $ d : pt : p : cls

parseEE :: IORef Int -> Bool -> Peg -> ClauseQ
parseEE glb th pg = do
	pgn <- newNewName glb "parse"
	listN <- newNewName glb "list"
	list1N <- newNewName glb "list1"
	optionalN <- newNewName glb "optional"
	pNames <- mapM (newNewName glb . \(n, _, _) -> n) pg

	pgenE <- varE pgn `appE` varE (mkName "initialPos")
	decs <- (:)
		<$> (funD pgn [parseE glb th pgn pNames pg])
		<*> (pSomes glb th listN list1N optionalN pNames pg)
	ld <- listDec listN list1N th
	od <- optionalDec optionalN th
	return $ Clause [] (NormalB pgenE) $ decs ++
		(if isListUsed pg then ld else []) ++
		(if isOptionalUsed pg then od else [])

dvCharsN, dvPosN :: Name
dvCharsN = mkName "derivsChars"
dvPosN = mkName "derivsPosition"

derivs :: Bool -> TypeQ -> TypeQ -> Peg -> DecQ
derivs _ src tkn pegg = dataD (cxt []) (mkName "Derivs") [] [
	recC (mkName "Derivs") $ map (derivs1 src) pegg ++ [
		varStrictType dvCharsN $ strictType notStrict $
			resultT src tkn,
		varStrictType dvPosN $ strictType notStrict $
			conT (mkName "Pos") `appT` src
	 ]
 ] []

derivs1 :: TypeQ -> Definition -> VarStrictTypeQ
derivs1 src (name, typ, _) =
	varStrictType (mkName name) $ strictType notStrict $ resultT src typ

throwErrorPackratMBody :: Bool -> ExpQ -> ExpQ -> ExpQ -> ExpQ -> ExpQ -> ExpQ
throwErrorPackratMBody th code msg com d ns = infixApp
	(varE (getsN th) `appE` varE dvPosN)
	(varE $ mkName ">>=") (infixApp
		(varE $ throwErrorN th)
		(varE $ mkName ".")
		(conE (mkName "ParseError")
			`appE` code
			`appE` msg
			`appE` com
			`appE` d
			`appE` ns))

resultT :: TypeQ -> TypeQ -> TypeQ
resultT src typ =
	conT eitherN `appT` pe `appT`
		(tupleT 2 `appT` typ `appT` conT (mkName "Derivs"))
	where
	pe = conT (mkName "ParseError")
		`appT` (conT (mkName "Pos") `appT` src)
		`appT` (conT $ mkName "Derivs")

parseT :: TypeQ -> Bool -> DecQ
parseT src _ = sigD (mkName "parse") $ arrowT
	`appT` src
	`appT` conT (mkName "Derivs")

newNewName :: IORef Int -> String -> Q Name
newNewName g base = do
	n <- runIO $ readIORef g
	runIO $ modifyIORef g succ
	newName (base ++ show n)
parseE :: IORef Int -> Bool -> Name -> [Name] -> Peg -> ClauseQ
parseE g th pgn pnames pegg = do
	tmps <- mapM (newNewName g) names
	parseE' g th pgn tmps pnames
	where
	names = map (\(n, _, _) -> n) pegg
parseE' :: IORef Int -> Bool -> Name -> [Name] -> [Name] -> ClauseQ
parseE' g th pgn tmps pnames = do
	chars <- newNewName g "chars"
	clause [varP $ mkName "pos", varP $ mkName "s"]
					(normalB $ varE $ mkName "d") $ [
		flip (valD $ varP $ mkName "d") [] $ normalB $ appsE $
			conE (mkName "Derivs") :
				map varE tmps ++ [varE chars, varE $ mkName "pos"]
	 ] ++ zipWith (parseE1 th) tmps pnames ++ [parseChar th pgn chars]

parseChar :: Bool -> Name -> Name -> DecQ
parseChar th pgn chars = flip (valD $ varP chars) [] $ normalB $
	varE (runStateTN th) `appE`
		caseE (varE (mkName "getToken") `appE` varE s) [
			match (justN th `conP` [tupP [varP c, varP s']])
				(normalB $ doE [
					noBindS $ varE (putN th) `appE`
						(parseGenE
							`appE` newPos
							`appE` varE s'),
					noBindS $ returnE `appE` varE c])
				[],
			match wildP
				(normalB $ newThrowQ th "" "end of input"
					(mkName "undefined")
					[] "")
				[]
		 ] `appE` varE (mkName "d")
	where
	newPos = varE (mkName "updatePos")
		`appE` varE (mkName "c")
		`appE` varE pos
	pos = mkName "pos"
	c = mkName "c"
	s = mkName "s"
	s' = mkName "s'"
	returnE = varE $ returnN th
	parseGenE = varE $ pgn
parseE1 :: Bool -> Name -> Name -> DecQ
parseE1 th tmp name = flip (valD $ varP tmp) [] $ normalB $
	varE (runStateTN th) `appE` varE name
		`appE` varE (mkName "d")

pSomes :: IORef Int -> Bool -> Name -> Name -> Name -> [Name] -> Peg -> DecsQ
pSomes g th lst lst1 opt = zipWithM $ pSomes1 g th lst lst1 opt

pSomes1 :: IORef Int -> Bool -> Name -> Name -> Name -> Name -> Definition -> DecQ
pSomes1 g th lst lst1 opt pname (_, _, sel) = flip (valD $ varP pname) [] $
	normalB $ pSomes1Sel g th lst lst1 opt sel

pSomes1Sel :: IORef Int -> Bool -> Name -> Name -> Name -> Selection -> ExpQ
pSomes1Sel g th lst lst1 opt sel =
	varE (mkName "foldl1") `appE` varE (mplusN th) `appE`
		listE (map (uncurry $ pSome_ g th lst lst1 opt) sel)

pSome_ :: IORef Int -> Bool -> Name -> Name -> Name -> [NameLeaf_] -> ExpQ -> ExpQ
pSome_ g th lst lst1 opt nls ret = fmap smartDoE $ do
	x <- mapM (transLeaf g th lst lst1 opt) nls
	r <- noBindS $ varE (returnN th) `appE` ret
	return $ concat x ++ [r]

afterCheck :: Bool -> ExpQ -> Name -> [String] -> String -> StmtQ
afterCheck th p d ns pc = do
	pp <- p
	noBindS $ varE (unlessN th) `appE` p `appE`
		newThrowQ th (show $ ppr pp) "not match: " d ns pc

beforeMatch :: Bool -> Name -> PatQ -> Name -> [String] -> String -> Q [Stmt]
beforeMatch th t n d ns nc = do
	nn <- n
	sequence [
		noBindS $ caseE (varE t) [
			flip (match $ varPToWild n) [] $ normalB $
				varE (returnN th) `appE` tupE [],
			flip (match wildP) [] $ normalB $
				newThrowQ th (show $ ppr nn) "not match pattern: "
					d ns nc
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

transReadFrom :: IORef Int -> Bool -> Name -> Name -> Name -> ReadFrom -> ExpQ
transReadFrom _ th _ _ _ FromToken = conE (stateTN' th) `appE` varE dvCharsN
transReadFrom _ th _ _ _ (FromVariable var) = conE (stateTN' th) `appE` varE (mkName var)
transReadFrom g th l l1 o (FromSelection sel) = pSomes1Sel g th l l1 o sel
transReadFrom g th l l1 o (FromList rf) = varE l `appE` transReadFrom g th l l1 o rf
transReadFrom g th l l1 o (FromList1 rf) = varE l1 `appE` transReadFrom g th l l1 o rf
transReadFrom g th l l1 o (FromOptional rf) = varE o `appE` transReadFrom g th l l1 o rf

transLeaf' :: IORef Int -> Bool -> Name -> Name -> Name -> NameLeaf -> Q [Stmt]
transLeaf' g th lst lst1 opt (NameLeaf (n, nc) rf (Just (p, pc))) = do
	t <- getNewName g "xx"
	d <- getNewName g "d"
	nn <- n
	case nn of
		WildP -> sequence [
			bindS (varP d) $ varE $ getN th,
			bindS wildP $ transReadFrom g th lst lst1 opt rf,
			afterCheck th p d (nameFromRF rf) pc
		 ]
		_	| notHaveOthers nn -> do
				bd <- bindS (varP d) $ varE $ getN th
				s <- bindS (varP t) $ transReadFrom g th lst lst1 opt rf
				m <- letS [flip (valD n) [] $ normalB $ varE t]
				c <- afterCheck th p d (nameFromRF rf) pc
				return $ bd : s : m : [c]
			| otherwise -> do
				bd <- bindS (varP d) $ varE $ getN th
				s <- bindS (varP t) $ transReadFrom g th lst lst1 opt rf
				m <- beforeMatch th t n d (nameFromRF rf) nc
				c <- afterCheck th p d (nameFromRF rf) pc
				return $ bd : s : m ++ [c]
	where
	notHaveOthers (VarP _) = True
	notHaveOthers (TupP pats) = all notHaveOthers pats
	notHaveOthers _ = False
transLeaf' g th lst lst1 opt (NameLeaf (n, nc) rf Nothing) = do
	t <- getNewName g "xx"
	d <- getNewName g "d"
	nn <- n
	case nn of
		WildP -> sequence [
			bindS wildP $ transReadFrom g th lst lst1 opt rf,
			noBindS $ varE (returnN th) `appE` tupE []
		 ]
		_	| notHaveOthers nn -> (: []) <$>
				bindS n (transReadFrom g th lst lst1 opt rf)
			| otherwise -> do
				bd <- bindS (varP d) $ varE $ getN th
				s <- bindS (varP t) $ transReadFrom g th lst lst1 opt rf
				m <- beforeMatch th t n d (nameFromRF rf) nc
				return $ bd : s : m
	where
	notHaveOthers (VarP _) = True
	notHaveOthers (TupP pats) = all notHaveOthers pats
	notHaveOthers _ = False

transLeaf :: IORef Int -> Bool -> Name -> Name -> Name -> NameLeaf_ -> Q [Stmt]
transLeaf g th lst lst1 opt (Here nl) = transLeaf' g th lst lst1 opt nl
transLeaf g th lst lst1 opt (After nl) = do
	d <- getNewName g "ddd"
	sequence [
		bindS (varP d) $ varE (getN th),
		noBindS $ smartDoE <$> transLeaf' g th lst lst1 opt nl,
		noBindS $ varE (putN th) `appE` varE d]
transLeaf g th lst lst1 opt (NotAfter nl@(NameLeaf _ rf _) com) = do
	d <- getNewName g "ddd"
	nls <- showNameLeaf nl
	sequence [
		bindS (varP d) $ varE (getN th),
		noBindS $ flipMaybeBody th
			(stringE nls)
			(stringE com)
			(varE d)
			(listE $ map stringE $ nameFromRF rf)
			(smartDoE <$> transLeaf' g th lst lst1 opt nl),
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
