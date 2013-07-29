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
import Data.Maybe
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

type ListNames = Lists -> Name

mkParseBody :: Bool -> Peg -> ClauseQ
mkParseBody th pgg = do
	glb <- runIO $ newIORef 0
	pgn <- newNewName glb "parse"
	ln <- (fromJust .) . flip lookup . zip [List, List1, Optional] <$>
		mapM (newNewName glb) ["list", "list1", "optional"]
	pNames <- mapM (newNewName glb . \(n, _, _) -> n) pgg
	tmps <- mapM (newNewName glb . \(n, _, _) -> n) pgg
	chars <- newNewName glb "chars"
	decs <- (:)
		<$> funD pgn [return $ parseE th pgn tmps chars pNames]
		<*> pSomes glb th ln pNames pgg
	return $ Clause [] (NormalB $ VarE pgn `AppE` VarE (mkName "initialPos")) $
		decs ++
		(if listUsed pgg then listDec (ln List) (ln List1) th else []) ++
		(if optionalUsed pgg then optionalDec (ln Optional) th else [])

parseE :: Bool -> Name -> [Name] -> Name -> [Name] -> Clause
parseE th pgn tmps chars pnames =
	Clause [VarP pos, VarP s] (NormalB $ VarE d) $ [
		flip (ValD $ VarP d) [] $ NormalB $ foldl1 AppE $
			ConE (mkName "Derivs") :
				map VarE tmps ++ [VarE chars, VarE pos]
	 ] ++ zipWith (parseE1 th) tmps pnames ++
		[parseChar th pgn chars pos s d]
	where
	pos = mkName "p"
	s = mkName "s"
	d = mkName "d"

parseChar :: Bool -> Name -> Name -> Name -> Name -> Name -> Dec
parseChar th pgn chars pos s d = flip (ValD $ VarP chars) [] $ NormalB $
	VarE (runStateTN th) `AppE`
		CaseE (VarE (mkName "getToken") `AppE` VarE s) [
			Match (justN th `ConP` [TupP [VarP c, VarP s']])
				(NormalB $ DoE [
					NoBindS $ VarE (putN th) `AppE`
						(parseGenE
							`AppE` newPos
							`AppE` VarE s'),
					NoBindS $ returnE `AppE` VarE c])
				[],
			flip (Match WildP) [] $ NormalB $
				newThrow th "" emsg (mkName "undefined") [] ""
		 ] `AppE` VarE d
	where
	emsg = "end of input"
	newPos = VarE (mkName "updatePos")
		`AppE` VarE (mkName "c")
		`AppE` VarE pos
	c = mkName "c"
	s' = mkName "s'"
	returnE = VarE $ returnN th
	parseGenE = VarE pgn

parseE1 :: Bool -> Name -> Name -> Dec
parseE1 th tmp name = flip (ValD $ VarP tmp) [] $ NormalB $
	VarE (runStateTN th) `AppE` VarE name `AppE` VarE (mkName "d")

pSomes :: IORef Int -> Bool -> ListNames -> [Name] -> Peg -> DecsQ
pSomes g th ln = zipWithM $ pSomes1 g th ln

pSomes1 :: IORef Int -> Bool -> ListNames -> Name -> Definition -> DecQ
pSomes1 g th ln pname (_, _, sel) =
	flip (valD $ varP pname) [] $ normalB $ pSomes1Sel g th ln sel

pSomes1Sel :: IORef Int -> Bool -> ListNames -> Selection -> ExpQ
pSomes1Sel g th ln esel = case esel of
	Left sel -> varE (mkName "foldl1") `appE` varE (mplusN th) `appE`
		listE (map (processExpressionHs g th ln) sel)
	Right sel -> varE (mkName "foldl1") `appE` varE (mplusN th) `appE`
		listE (zipWith
			(flip (putLeftRight $ length sel) .
				processPlainExpressionHs g th ln)
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
	IORef Int -> Bool -> ListNames -> Expression -> ExpQ
processExpressionHs g th ln exhs = do
	let (expr, ret) = exhs
	fmap smartDoE $ do
		x <- forM expr $ \(ha, nl) -> do
			let	nls = show $ pprCheck nl
				(_, rf, _) = nl
			processHA g th ha (return nls) (nameFromRF rf) $
				transLeaf g th ln nl
		r <- noBindS $ varE (returnN th) `appE` return ret
		return $ concat x ++ [r]

processPlainExpressionHs ::
	IORef Int -> Bool -> ListNames -> PlainExpression -> ExpQ
processPlainExpressionHs g th ln rfs =
	foldl (\x y -> infixApp x appApply y)
		(returnEQ `appE` mkTupleE g (map fst rfs)) $
			map (transHAReadFrom g th ln) rfs

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
		return (newThrow th (show $ ppr pp) "not match: " d ns pc)

beforeMatch :: Bool -> Name -> PatQ -> Name -> Q [String] -> String -> Q [Stmt]
beforeMatch th t n d names nc = do
	nn <- n
	ns <- names
	sequence [
		noBindS $ caseE (varE t) [
			flip (match $ varPToWild n) [] $ normalB $
				varE (returnN th) `appE` tupE [],
			flip (match wildP) [] $ normalB $ return $
				newThrow th (show $ ppr nn) "not match pattern: "
					d ns nc
		 ],
		letS [flip (valD n) [] $ normalB $ varE t],
		noBindS $ varE (returnN th) `appE` tupE []
	 ]

transHAReadFrom ::
	IORef Int -> Bool -> ListNames -> (Lookahead, ReadFrom) -> ExpQ
transHAReadFrom g th ln (ha, rf) =
	smartDoE <$> processHA g th ha (return "") (nameFromRF rf)
		(fmap (: []) $ noBindS $ transReadFrom g th ln rf)

transReadFrom :: IORef Int -> Bool -> ListNames -> ReadFrom -> ExpQ
transReadFrom _ th _ (FromVariable Nothing) =
	conE (stateTN' th) `appE` varE dvCharsN
transReadFrom _ th _ (FromVariable (Just var)) =
	conE (stateTN' th) `appE` varE (mkName var)
transReadFrom g th ln (FromSelection sel) = pSomes1Sel g th ln sel
transReadFrom g th ln (FromL List rf) =
	varE (ln List) `appE` transReadFrom g th ln rf
transReadFrom g th ln (FromL List1 rf) =
	varE (ln List1) `appE` transReadFrom g th ln rf
transReadFrom g th ln (FromL Optional rf) =
	varE (ln Optional) `appE` transReadFrom g th ln rf

transLeaf :: IORef Int -> Bool -> ListNames -> Check -> Q [Stmt]
transLeaf g th ln ((n, nc), rf, Just (p, pc)) = do
	t <- newNewName g "xx"
	d <- newNewName g "d"
	case n of
		WildP -> sequence [
			bindS (varP d) $ varE $ getN th,
			bindS wildP $ transReadFrom g th ln rf,
			afterCheck th (return p) d (nameFromRF rf) pc
		 ]
		_	| notHaveOthers n -> do
				bd <- bindS (varP d) $ varE $ getN th
				s <- bindS (varP t) $ transReadFrom g th ln rf
				m <- letS [flip (valD $ return n) [] $
					normalB $ varE t]
				c <- afterCheck th (return p) d (nameFromRF rf) pc
				return $ bd : s : m : [c]
			| otherwise -> do
				bd <- bindS (varP d) $ varE $ getN th
				s <- bindS (varP t) $
					transReadFrom g th ln rf
				m <- beforeMatch th t (return n) d
					(nameFromRF rf) nc
				c <- afterCheck th (return p) d (nameFromRF rf) pc
				return $ bd : s : m ++ [c]
	where
	notHaveOthers (VarP _) = True
	notHaveOthers (TupP pats) = all notHaveOthers pats
	notHaveOthers _ = False
transLeaf g th ln ((n, nc), rf, Nothing) = do
	t <- newNewName g "xx"
	d <- newNewName g "d"
	case n of
		WildP -> sequence [
			bindS wildP $ transReadFrom g th ln rf,
			noBindS $ varE (returnN th) `appE` tupE []
		 ]
		_	| notHaveOthers n -> (: []) <$>
				bindS (return n)
					(transReadFrom g th ln rf)
			| otherwise -> do
				bd <- bindS (varP d) $ varE $ getN th
				s <- bindS (varP t) $
					transReadFrom g th ln rf
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
	d <- newNewName g "ddd"
	sequence [
		bindS (varP d) $ varE (getN th),
		bindS wildP $ smartDoE <$> act,
		noBindS $ varE (putN th) `appE` varE d]
processHA g th (NAhead com) nlss names act = do
	d <- newNewName g "ddd"
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

dvCharsN, dvPosN :: Name
dvCharsN = mkName "char"
dvPosN = mkName "position"

throwErrorPackratMBody :: Bool -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp
throwErrorPackratMBody th code msg com d ns = InfixE
	(Just $ VarE (getsN th) `AppE` VarE dvPosN)
	(VarE $ mkName ">>=")
	(Just $ InfixE
		(Just $ VarE $ throwErrorN th)
		(VarE $ mkName ".")
		(Just $ VarE (mkName "mkParseError")
			`AppE` code
			`AppE` msg
			`AppE` com
			`AppE` d
			`AppE` ns))

newNewName :: IORef Int -> String -> Q Name
newNewName g base = do
	n <- runIO $ readIORef g
	runIO $ modifyIORef g succ
	newName $ base ++ show n

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
		`appE` (throwErrorPackratMBody th
			<$> infixApp (litE $ charL '!') (conE $ mkName ":") code
			<*> stringE "not match: " <*> com <*> d <*> ns)
 ]	where
	actionReturnFalse = infixApp act (varE $ mkName ">>")
		(varE (mkName "return") `appE` conE (mkName "False"))
	constReturnTrue = varE (mkName "const") `appE` 
		(varE (mkName "return") `appE` conE (mkName "True"))

newThrow :: Bool -> String -> String -> Name -> [String] -> String -> Exp
newThrow th code msg d ns com =
	throwErrorPackratMBody th
		(LitE $ StringL code)
		(LitE $ StringL msg)
		(LitE $ StringL com)
		(VarE d)
		(ListE $ map (LitE . StringL) ns)

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
