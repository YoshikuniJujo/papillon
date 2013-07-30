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
-- import "monads-tf" Control.Monad.Identity
import "monads-tf" Control.Monad.Error

import Control.Applicative

import Text.Papillon.Parser
import Data.Maybe
import Data.IORef

import Text.Papillon.List

import System.IO.Unsafe

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

type Variables = [(String, [Name])]

newVariable :: IORef Int -> Variables -> String -> Q Variables
newVariable g vs n = (: vs) . (n ,) <$> getVars
	where
	getVars = runIO $ unsafeInterleaveIO $ (:) <$>
		runQ (newNewName g n) <*> runQ getVars

getVariable :: Variables -> String -> Name
getVariable = ((head . fromJust) .) . flip lookup

nextVariable :: Variables -> String -> Variables
nextVariable vs n = (n, tail $ fromJust $ lookup n vs) : vs

mkParseBody :: Bool -> Peg -> ClauseQ
mkParseBody th pgg = do
	glb <- runIO $ newIORef 0
	vars <- foldM (newVariable glb) [] [
		"parse", "chars", "pos", "d", "c", "s", "s'", "x", "t",
		"ddd"]
	let	pgn = getVariable vars "parse"
		vars' = nextVariable vars "d"
	ln <- (fromJust .) . flip lookup . zip [List, List1, Optional] <$>
		mapM (newNewName glb) ["list", "list1", "optional"]
	pNames <- mapM (newNewName glb . \(n, _, _) -> n) pgg
	tmps <- mapM (newNewName glb . \(n, _, _) -> n) pgg
	core <- funD pgn [return $ mkParseCore th vars tmps pNames]
	let decs = flip evalState vars' $ zipWithM (pSomes th ln) pNames pgg
	return $ Clause [] (NormalB $ VarE pgn `AppE` VarE (mkName "initialPos")) $
		core : decs ++
		(if listUsed pgg then listDec (ln List) (ln List1) th else []) ++
		(if optionalUsed pgg then optionalDec (ln Optional) th else [])

mkParseCore :: Bool -> Variables -> [Name] -> [Name] -> Clause
mkParseCore th vars tmps pnames =
	Clause [VarP pos, VarP s] (NormalB $ VarE d) $ [
		flip (ValD $ VarP d) [] $ NormalB $ foldl1 AppE $
			ConE (mkName "Derivs") :
				map VarE tmps ++ [VarE chars, VarE pos]
	 ] ++ zipWith pe1 tmps pnames ++ [parseChar th vars pgn chars pos s d]
	where
	pgn = getVariable vars "parse"
	chars = getVariable vars "chars"
	pos = getVariable vars "pos"
	s = getVariable vars "s"
	d = getVariable vars "d"
	pe1 tmp name = flip (ValD $ VarP tmp) [] $ NormalB $
		VarE (runStateTN th) `AppE` VarE name `AppE` VarE d

parseChar :: Bool -> Variables -> Name -> Name -> Name -> Name -> Name -> Dec
parseChar th vars pgn chars pos s d = flip (ValD $ VarP chars) [] $ NormalB $
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
		`AppE` VarE c
		`AppE` VarE pos
	c = getVariable vars "c"
	s' = getVariable vars "s'"
	returnE = VarE $ returnN th
	parseGenE = VarE pgn

type VarMonad = State Variables

pSomes :: Bool -> ListNames -> Name -> Definition -> VarMonad Dec
pSomes th ln pname (_, _, sel) =
	flip (ValD $ VarP pname) [] . NormalB <$> pSomes1Sel th ln sel

pSomes1Sel :: Bool -> ListNames -> Selection -> VarMonad Exp
pSomes1Sel th ln esel = do
	(VarE (mkName "foldl1") `AppE` VarE (mplusN th) `AppE`) . ListE <$>
		case esel of
			Left sel -> mapM (processExpressionHs th ln) sel
			Right sel -> zipWithM (flip (putLeftRightS $ length sel) .
					processPlainExpressionHs th ln) sel [0..]
	where
	putLeftRightS a h exs = StateT $ \s -> do
		(ex, s') <- runStateT exs s
		let ret = putLeftRight a h ex
		return (ret, s')
	putLeftRight 1 0 ex = ex
	putLeftRight _ 0 ex = leftE `AppE` ex
	putLeftRight l n ex
		| n == l - 1 = rightE `AppE` putLeftRight (l - 1) (n - 1) ex
		| otherwise = rightE `AppE` putLeftRight l (n - 1) ex
	rightE = VarE (mkName "fmap") `AppE` ConE (mkName "Right")
	leftE = VarE (mkName "fmap") `AppE` ConE (mkName "Left")

processExpressionHs :: Bool -> ListNames -> Expression -> VarMonad Exp
processExpressionHs th ln (expr, ret) = do
	smartDoE <$> do
		x <- forM expr $ \(ha, nl) -> do
			let	nls = show $ pprCheck nl
				(_, rf, _) = nl
			processHA th ha nls (nameFromRF rf) $
				transLeaf th ln nl
		r <- return $ NoBindS $ VarE (returnN th) `AppE` ret
		modify $ flip nextVariable "d"
		return $ concat x ++ [r]

processPlainExpressionHs :: Bool -> ListNames -> PlainExpression -> VarMonad Exp
processPlainExpressionHs th ln rfs = do
	vars <- get
	exps <- mapM (transHAReadFrom th ln) rfs
	return $ foldl (\x y -> InfixE (Just x) appApply (Just y))
		(returnEQ `AppE` mkTuple2 (map fst rfs) vars) exps
	where
	mkTuple2 has vars = let
		names = take (length has) $ fromJust $ lookup "x" vars in
		LamE (mkPat names has) $ TupE $ mkExp names has
	mkPat _ [] = []
	mkPat (n : ns) (Here : hs) = VarP n : mkPat ns hs
	mkPat (_ : ns) (_ : hs) = WildP : mkPat ns hs
	mkPat _ _ = error "bad"
	mkExp _ [] = []
	mkExp (n : ns) (Here : hs) = VarE n : mkExp ns hs
	mkExp (_ : ns) (_ : hs) = mkExp ns hs
	mkExp _ _ = error "bad"
	appApply = VarE $ mkName "<*>"
	returnEQ = VarE $ mkName "return"

afterCheck :: Bool -> Exp -> Name -> [String] -> String -> Stmt
afterCheck th pp d ns pc = do
	NoBindS $ VarE (unlessN th) `AppE` pp `AppE`
		(newThrow th (show $ ppr pp) "not match: " d ns pc)

beforeMatch :: Bool -> Name -> Pat -> Name -> [String] -> String -> [Stmt]
beforeMatch th t nn d ns nc = [
	NoBindS $ CaseE (VarE t) [
		flip (Match $ vpw nn) [] $ NormalB $
			VarE (returnN th) `AppE` TupE [],
		flip (Match WildP) [] $ NormalB $
			newThrow th (show $ ppr nn) "not match pattern: " d ns nc],
	LetS $ [ValD nn (NormalB $ VarE t) []],
	NoBindS $ VarE (returnN th) `AppE` TupE []]
	where
	vpw (VarP _) = WildP
	vpw (ConP n ps) = ConP n $ map vpw ps
	vpw (InfixP p1 n p2) = InfixP (vpw p1) n (vpw p2)
	vpw (UInfixP p1 n p2) = InfixP (vpw p1) n (vpw p2)
	vpw (ListP ps) = ListP $ vpw `map` ps
	vpw (TupP ps) = TupP $ vpw `map` ps
	vpw o = o

transHAReadFrom :: Bool -> ListNames -> (Lookahead, ReadFrom) -> VarMonad Exp
transHAReadFrom th ln (ha, rf) = do
	modify $ flip nextVariable "d"
	smartDoE <$> processHA th ha "" (nameFromRF rf)
		((: []) . NoBindS <$> transReadFrom th ln rf)

transReadFrom :: Bool -> ListNames -> ReadFrom -> VarMonad Exp
transReadFrom th _ (FromVariable Nothing) = return $
	ConE (stateTN' th) `AppE` VarE dvCharsN
transReadFrom th _ (FromVariable (Just var)) = return $
	ConE (stateTN' th) `AppE` VarE (mkName var)
transReadFrom th ln (FromSelection sel) = pSomes1Sel th ln sel
transReadFrom th ln (FromL List rf) =
	(VarE (ln List) `AppE`) <$> transReadFrom th ln rf
transReadFrom th ln (FromL List1 rf) =
	(VarE (ln List1) `AppE`) <$> transReadFrom th ln rf
transReadFrom th ln (FromL Optional rf) =
	(VarE (ln Optional) `AppE`) <$> transReadFrom th ln rf

transLeaf :: Bool -> ListNames -> Check -> VarMonad [Stmt]
transLeaf th ln ((n, nc), rf, Just (p, pc)) = do
	t <- gets $ flip getVariable "t"
	d <- gets $ flip  getVariable "d"
	modify $ flip nextVariable "t"
	modify $ flip nextVariable "d"
	case n of
		WildP -> ((BindS (VarP d) (VarE $ getN th) :) .
				(: [afterCheck th p d (nameFromRF rf) pc])) .
			BindS WildP <$> transReadFrom th ln rf
		_	| notHaveOthers n -> do
				let	bd = BindS (VarP d) $ VarE $ getN th
					m = LetS [ValD n (NormalB $ VarE t) []]
					c = afterCheck th p d (nameFromRF rf) pc
				s <- BindS (VarP t) <$> transReadFrom th ln rf
				return [bd, s, m, c]
			| otherwise -> do
				let	bd = BindS (VarP d) $ VarE $ getN th
					m = beforeMatch th t n d (nameFromRF rf) nc
					c = afterCheck th p d (nameFromRF rf) pc
				s <- BindS (VarP t) <$> transReadFrom th ln rf
				return $ [bd, s]  ++ m ++ [c]
	where
	notHaveOthers (VarP _) = True
	notHaveOthers (TupP pats) = all notHaveOthers pats
	notHaveOthers _ = False
transLeaf th ln ((n, nc), rf, Nothing) = do
	t <- gets $ flip getVariable "t"
	d <- gets $ flip getVariable "d"
	modify $ flip nextVariable "d"
	modify $ flip nextVariable "t"
	case n of
		WildP -> sequence [
			BindS WildP <$> transReadFrom th ln rf,
			return $ NoBindS $ VarE (returnN th) `AppE` TupE []
		 ]
		_	| notHaveOthers n ->
				(: []) . BindS n <$> transReadFrom th ln rf
			| otherwise -> do
				let	bd = BindS (VarP d) $ VarE $ getN th
					m = beforeMatch th t n d (nameFromRF rf) nc
				s <- BindS (VarP t) <$> transReadFrom th ln rf
				return $ bd : s : m
	where
	notHaveOthers (VarP _) = True
	notHaveOthers (TupP pats) = all notHaveOthers pats
	notHaveOthers _ = False

processHA :: Bool -> Lookahead -> String -> [String] -> VarMonad [Stmt] ->
	VarMonad [Stmt]
processHA _ Here _ _ act = act
processHA th Ahead _ _ act = do
	d <- gets $ flip getVariable "ddd"
	modify $ flip nextVariable "ddd"
	r <- act
	return [
		BindS (VarP d) $ VarE (getN th),
		BindS WildP $ smartDoE r,
		NoBindS $ VarE (putN th) `AppE` VarE d]
processHA th (NAhead com) nls ns act = do
	d <- gets $ flip getVariable "ddd"
	modify $ flip nextVariable "ddd"
	r <- act
	return [
		BindS (VarP d) $ VarE (getN th),
		NoBindS $ flipMaybeBody th
			(LitE $ StringL nls)
			(LitE $ StringL com)
			(VarE d)
			(ListE $ map (LitE . StringL) ns)
			(smartDoE r),
		NoBindS $ VarE (putN th) `AppE` VarE d]

flipMaybeBody :: Bool -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp
flipMaybeBody th code com d ns act = DoE [
	BindS (VarP $ mkName "err") $ InfixE
		(Just actionReturnFalse)
		(VarE $ catchErrorN th)
		(Just constReturnTrue),
	NoBindS $ VarE (unlessN th)
		`AppE` VarE (mkName "err")
		`AppE` (throwErrorPackratMBody th
			(InfixE (Just $ LitE $ CharL '!') (ConE $ mkName ":")
				(Just code))
			(LitE $ StringL "not match: ") com d ns)
 ]	where
	actionReturnFalse = InfixE (Just act) (VarE $ mkName ">>")
		(Just $ VarE (mkName "return") `AppE` ConE (mkName "False"))
	constReturnTrue = VarE (mkName "const") `AppE` 
		(VarE (mkName "return") `AppE` ConE (mkName "True"))

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
