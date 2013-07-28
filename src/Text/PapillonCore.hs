{-# LANGUAGE TemplateHaskell, PackageImports, TypeFamilies, FlexibleContexts,
	FlexibleInstances, TupleSections #-}

module Text.PapillonCore (
	-- * For Text.Papillon library
	papillonCore,

	Source(..),
	SourceList(..),

	-- ** For parse error message
	ParseError,
	mkParseError,
	peDerivs,
	peReading,
	peMessage,
	peCode,
	peComment,
	pePosition,
	pePositionS,
	Pos(..),
	ListPos(..),

	-- * For papillon command
	papillonFile,
	PPragma(..),
	ModuleName,
	Exports,
	Code,
	(<*>)
) where

import Language.Haskell.TH
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error

import Control.Applicative

import Text.Papillon.Parser
import Data.IORef

import Text.Papillon.List

papillonCore :: String -> DecsQ
papillonCore str = case peg $ parse str of
	Right (stpegq, _) -> do
		(src, parsed) <- stpegq =<< runIO (newIORef 0)
		decParsed True src parsed
	Left err -> error $ "parse error: " ++ showParseError err

papillonFile :: String -> Q ([PPragma], ModuleName, Maybe Exports, Code, DecsQ, Code)
papillonFile str = case pegFile $ parse str of
	Right (pegfileq, _) -> do
		g <- runIO $ newIORef 0
		(prgm, mn, ppp, pp, (src, parsed), atp) <- pegfileq g
		let	lu = listUsed parsed
			ou = optionalUsed parsed
		let addApplicative =
			if lu || ou then "import Control.Applicative\n" else ""
		return (prgm, mn, ppp, addApplicative ++ pp, decs src parsed, atp)
		where
		decs = decParsed False
	Left err -> error $ "parse error: " ++ showParseError err

decParsed :: Bool -> Type -> Peg -> DecsQ
decParsed th src parsed = do
	let	d = derivs src parsed
		pt = SigD (mkName "parse") $ src `arrT` ConT (mkName "Derivs")
	p <- funD (mkName "parse") [mkParseBody th parsed]
	return [d, pt, p]

derivs :: Type -> Peg -> Dec
derivs src pg = DataD [] (mkName "Derivs") [] [
	RecC (mkName "Derivs") $ map derivs1 pg ++ [
		(dvCharsN, NotStrict, resultT tkn),
		(dvPosN, NotStrict, ConT (mkName "Pos") `AppT` src)
	 ]] []
	where
	tkn = ConT (mkName "Token") `AppT` src
	derivs1 (name, Just t, _) = (mkName name, NotStrict, resultT t)
	derivs1 (name, Nothing, sel) =
		(mkName name, NotStrict, resultT $ selectionType pg tkn sel)
	resultT typ = ConT (mkName "Either")
		`AppT` (ConT (mkName "ParseError")
			`AppT` (ConT (mkName "Pos") `AppT` src)
			`AppT` ConT (mkName "Derivs"))
		`AppT` (TupleT 2 `AppT` typ `AppT` ConT (mkName "Derivs"))

mkParseBody :: Bool -> Peg -> ClauseQ
mkParseBody th pgg = do
	glb <- runIO $ newIORef 0
	pgn <- newNewName glb "parse"
	listN <- newNewName glb "list"
	list1N <- newNewName glb "list1"
	optionalN <- newNewName glb "optional"
	pNames <- mapM (newNewName glb . getDefinitionName) pgg

	pgenE <- varE pgn `appE` varE (mkName "initialPos")
	decs <- (:)
		<$> funD pgn [parseE glb th pgn pNames pgg]
		<*> pSomes glb th listN list1N optionalN pNames pgg
	let	lu = listUsed pgg
		ou = optionalUsed pgg
	ld <- listDec listN list1N th
	od <- optionalDec optionalN th
	return $ Clause [] (NormalB pgenE) $
		decs ++ (if lu then ld else []) ++ (if ou then od else [])

dvCharsN, dvPosN :: Name
dvCharsN = mkName "char"
dvPosN = mkName "position"

throwErrorPackratMBody :: Bool -> ExpQ -> ExpQ -> ExpQ -> ExpQ -> ExpQ -> ExpQ
throwErrorPackratMBody th code msg com d ns = infixApp
	(varE (getsN th) `appE` varE dvPosN)
	(varE $ mkName ">>=") (infixApp
		(varE $ throwErrorN th)
		(varE $ mkName ".")
		(varE (mkName "mkParseError")
			`appE` code
			`appE` msg
			`appE` com
			`appE` d
			`appE` ns))

newNewName :: IORef Int -> String -> Q Name
newNewName g base = do
	n <- runIO $ readIORef g
	runIO $ modifyIORef g succ
	newName (base ++ show n)
parseE :: IORef Int -> Bool -> Name -> [Name] -> Peg -> ClauseQ
parseE g th pgn pnames pg = do
	tmps <- mapM (newNewName g . getDefinitionName) pg
	parseE' g th pgn tmps pnames $ map (mkName . getDefinitionName) pg

getDefinitionName :: Definition -> String
getDefinitionName (n, _, _) = n

parseE' :: IORef Int -> Bool -> Name -> [Name] -> [Name] -> [Name] -> ClauseQ
parseE' g th pgn tmps pnames names = do
	chars <- newNewName g "chars"
	clause [varP $ mkName "pos", varP $ mkName "s"]
					(normalB $ varE $ mkName "d") $ [
		flip (valD $ varP $ mkName "d") [] $ normalB $ appsE $
			conE (mkName "Derivs") :
				map varE tmps ++ [varE chars, varE $ mkName "pos"]
	 ] ++ zipWith3 (parseE1 th) tmps pnames names ++ [parseChar th pgn chars]

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
	parseGenE = varE pgn
parseE1 :: Bool -> Name -> Name -> Name -> DecQ
parseE1 th tmp name _ = flip (valD $ varP tmp) [] $ normalB $
	varE (runStateTN th) `appE` varE name `appE` varE (mkName "d")

pSomes :: IORef Int -> Bool -> Name -> Name -> Name -> [Name] -> Peg -> DecsQ
pSomes g th lst lst1 opt = zipWithM (pSomes1 g th lst lst1 opt)

pSomes1 :: IORef Int -> Bool -> Name -> Name -> Name -> Name -> Definition -> DecQ
pSomes1 g th lst lst1 opt pname (_, _, sel) =
	flip (valD $ varP pname) [] $ normalB $ pSomes1Sel g th lst lst1 opt sel

pSomes1Sel :: IORef Int -> Bool -> Name -> Name -> Name -> Selection -> ExpQ
pSomes1Sel g th lst lst1 opt esel = case esel of
	Left sel -> varE (mkName "foldl1") `appE` varE (mplusN th) `appE`
		listE (map (processExpressionHs g th lst lst1 opt) sel)
	Right sel -> varE (mkName "foldl1") `appE` varE (mplusN th) `appE`
		listE (zipWith
			(flip (putLeftRight $ length sel) .
				processPlainExpressionHs g th lst lst1 opt)
			sel [0..])

putLeftRight :: Int -> Int -> ExpQ -> ExpQ
putLeftRight 1 0 ex = ex
putLeftRight _ 0 ex = leftE `appE` ex
putLeftRight l n ex
	| n == l - 1 = rightE `appE` putLeftRight (l - 1) (n - 1) ex
	| otherwise = rightE `appE` putLeftRight l (n - 1) ex

rightE, leftE :: ExpQ
rightE = varE (mkName "fmap") `appE` conE (mkName "Right")
leftE = varE (mkName "fmap") `appE` conE (mkName "Left")

processExpressionHs ::
	IORef Int -> Bool -> Name -> Name -> Name -> Expression -> ExpQ
processExpressionHs g th lst lst1 opt exhs = do
	let (expr, ret) = exhs
	fmap smartDoE $ do
		x <- forM expr $ \(ha, nl) -> do
			let	nls = show $ pprCheck nl
				(_, rf, _) = nl
			processHA g th ha (return nls) (nameFromRF rf) $
				transLeaf g th lst lst1 opt nl
		r <- noBindS $ varE (returnN th) `appE` return ret
		return $ concat x ++ [r]

processPlainExpressionHs ::
	IORef Int -> Bool -> Name -> Name -> Name -> PlainExpression -> ExpQ
processPlainExpressionHs g th lst lst1 opt rfs =
	foldl (\x y -> infixApp x appApply y)
		(returnEQ `appE` mkTupleE g (map fst rfs)) $
			map (transHAReadFrom g th lst lst1 opt) rfs

mkTupleE :: IORef Int -> [Lookahead] -> ExpQ
mkTupleE g has = do
	names <- replicateM (length has) $ newNewName g "x"
	lamE (mkPat names has) $ tupE $ mkExp names has
	where
	mkPat _ [] = []
	mkPat (n : ns) (Here : hs) = varP n : mkPat ns hs
	mkPat (_ : ns) (_ : hs) = wildP : mkPat ns hs
	mkPat _ _ = error "bad"
	mkExp _ [] = []
	mkExp (n : ns) (Here : hs) = varE n : mkExp ns hs
	mkExp (_ : ns) (_ : hs) = mkExp ns hs
	mkExp _ _ = error "bad"

appApply :: ExpQ
appApply = varE $ mkName "<*>"

returnEQ :: ExpQ
returnEQ = varE $ mkName "return"

afterCheck :: Bool -> ExpQ -> Name -> Q [String] -> String -> StmtQ
afterCheck th p d names pc = do
	pp <- p
	ns <- names
	noBindS $ varE (unlessN th) `appE` p `appE`
		newThrowQ th (show $ ppr pp) "not match: " d ns pc

beforeMatch :: Bool -> Name -> PatQ -> Name -> Q [String] -> String -> Q [Stmt]
beforeMatch th t n d names nc = do
	nn <- n
	ns <- names
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

transHAReadFrom ::
	IORef Int -> Bool -> Name -> Name -> Name -> (Lookahead, ReadFrom) -> ExpQ
transHAReadFrom g th lst lst1 opt (ha, rf) = smartDoE <$>
	processHA g th ha (return "") (nameFromRF rf)
		(fmap (: []) $ noBindS $ transReadFrom g th lst lst1 opt rf)

transReadFrom :: IORef Int -> Bool -> Name -> Name -> Name -> ReadFrom -> ExpQ
transReadFrom _ th _ _ _ (FromVariable Nothing) =
	conE (stateTN' th) `appE` varE dvCharsN
transReadFrom _ th _ _ _ (FromVariable (Just var)) =
	conE (stateTN' th) `appE` varE (mkName var)
transReadFrom g th l l1 o (FromSelection sel) = pSomes1Sel g th l l1 o sel
transReadFrom g th l l1 o (FromL List rf) = varE l `appE` transReadFrom g th l l1 o rf
transReadFrom g th l l1 o (FromL List1 rf) = varE l1 `appE` transReadFrom g th l l1 o rf
transReadFrom g th l l1 o (FromL Optional rf) = varE o `appE` transReadFrom g th l l1 o rf

mkTD :: IORef Int -> Q (Name, Name)
mkTD g = do
	t <- getNewName g "xx"
	d <- getNewName g "d"
	return (t, d)

transLeaf :: IORef Int -> Bool -> Name -> Name -> Name -> Check -> Q [Stmt]
transLeaf g th lst lst1 opt ((n, nc), rf, Just (p, pc)) = do
	(t, d) <- mkTD g
	let nn = n
	case nn of
		WildP -> sequence [
			bindS (varP d) $ varE $ getN th,
			bindS wildP $ transReadFrom g th lst lst1 opt rf,
			afterCheck th (return p) d (nameFromRF rf) pc
		 ]
		_	| notHaveOthers nn -> do
				bd <- bindS (varP d) $ varE $ getN th
				s <- bindS (varP t) $ transReadFrom g th lst lst1 opt rf
				m <- letS [flip (valD $ return n) [] $
					normalB $ varE t]
				c <- afterCheck th (return p) d (nameFromRF rf) pc
				return $ bd : s : m : [c]
			| otherwise -> do
				bd <- bindS (varP d) $ varE $ getN th
				s <- bindS (varP t) $
					transReadFrom g th lst lst1 opt rf
				m <- beforeMatch th t (return n) d
					(nameFromRF rf) nc
				c <- afterCheck th (return p) d (nameFromRF rf) pc
				return $ bd : s : m ++ [c]
	where
	notHaveOthers (VarP _) = True
	notHaveOthers (TupP pats) = all notHaveOthers pats
	notHaveOthers _ = False
transLeaf g th lst lst1 opt ((n, nc), rf, Nothing) = do
	(t, d) <- mkTD g
	let nn = n
	case nn of
		WildP -> sequence [
			bindS wildP $ transReadFrom g th lst lst1 opt rf,
			noBindS $ varE (returnN th) `appE` tupE []
		 ]
		_	| notHaveOthers nn -> (: []) <$>
				bindS (return n)
					(transReadFrom g th lst lst1 opt rf)
			| otherwise -> do
				bd <- bindS (varP d) $ varE $ getN th
				s <- bindS (varP t) $
					transReadFrom g th lst lst1 opt rf
				m <- beforeMatch th t (return n) d
					(nameFromRF rf) nc
				return $ bd : s : m
	where
	notHaveOthers (VarP _) = True
	notHaveOthers (TupP pats) = all notHaveOthers pats
	notHaveOthers _ = False

processHA :: IORef Int -> Bool -> Lookahead -> Q String -> Q [String] ->
	Q [Stmt] -> Q [Stmt]
processHA _ _ Here _ _ act = act
processHA g th Ahead _ _ act = do
	d <- getNewName g "ddd"
	sequence [
		bindS (varP d) $ varE (getN th),
		bindS wildP $ smartDoE <$> act,
		noBindS $ varE (putN th) `appE` varE d]
processHA g th (NAhead com) nlss names act = do
	d <- getNewName g "ddd"
	ns <- names
	nls <- nlss
	sequence [
		bindS (varP d) $ varE (getN th),
		noBindS $ flipMaybeBody th
			(stringE nls)
			(stringE com)
			(varE d)
			(listE $ map stringE ns)
			(smartDoE <$> act),
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

listUsed, optionalUsed :: Peg -> Bool
listUsed = any $ sel . \(_, _, s) -> s
	where
	sel = either	(any $ any (rf . (\(_, r, _) -> r) . snd) . fst)
			(any $ any $ rf . snd)
	rf (FromL List _) = True
	rf (FromL List1 _) = True
	rf (FromSelection s) = sel s
	rf _ = False
optionalUsed = any $ sel . \(_, _, s) -> s
	where
	sel = either	(any $ any (rf . (\(_, r, _) -> r) . snd) . fst)
			(any $ any $ rf . snd)
	rf (FromL Optional _) = True
	rf (FromSelection s) = sel s
	rf _ = False

arrT :: Type -> Type -> Type
arrT a r = ArrowT `AppT` a `AppT` r

showParseError :: ParseError (Pos String) Derivs -> String
showParseError pe =
	unwords (map (showReading d) ns) ++ (if null ns then "" else " ") ++
	m ++ c ++ " at position: " ++ show p
	where
	[c, m, _] = ($ pe) `map` [peCode, peMessage, peComment]
	ns = peReading pe
	d = peDerivs pe
	p = pePositionS pe

showReading :: Derivs -> String -> String
showReading d "char" = case char d of
	Right (c, _) -> show c
	Left _ -> error "bad"
showReading _ n = "yet: " ++ n

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
