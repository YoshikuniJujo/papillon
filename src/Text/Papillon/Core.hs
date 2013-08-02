{-# LANGUAGE TemplateHaskell, PackageImports, TypeFamilies, FlexibleContexts,
	FlexibleInstances, TupleSections #-}

module Text.Papillon.Core (
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
	(<*>),
	(<$>),
	runError
) where

import Language.Haskell.TH hiding (infixApp, doE)
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Identity
import "monads-tf" Control.Monad.Error

import Control.Applicative

import Text.Papillon.Parser
import Data.Maybe
import Data.IORef

import Text.Papillon.List

import System.IO.Unsafe

dvPosN :: Name
dvPosN = mkName "position"

papillonCore :: String -> DecsQ
papillonCore str = case flip evalState (0, 0) $ runErrorT $ peg $ parse str of
	Right (stpegq, _) -> do
		let (monad, src, parsed) = stpegq
		decParsed True monad src parsed
	Left err -> error $ "parse error: " ++ showParseError err

papillonFile :: String ->
	Q ([PPragma], ModuleName, Maybe Exports, Code, DecsQ, Code)
papillonFile str = case flip evalState (0, 0) $ runErrorT $ pegFile $ parse str of
	Right (pegfileq, _) -> do
		let	(prgm, mn, ppp, pp, (monad, src, parsed), atp) = pegfileq
			lu = listUsed parsed
			ou = optionalUsed parsed
			addApplicative = if lu || ou
				then "import Control.Applicative\n" else ""
			addIdentity = if isJust monad then ""
				else "import \"monads-tf\" Control.Monad.Identity\n"
		return (prgm, mn, ppp,
			addApplicative ++ addIdentity ++ pp,
			decs monad src parsed, atp)
		where
		decs = decParsed False
	Left err -> error $ "parse error: " ++ showParseError err

decParsed :: Bool -> Maybe Type -> Type -> Peg -> DecsQ
decParsed th monad src parsed = do
	let	d = derivs th (fromMaybe (ConT $ identityN th) monad) src parsed
		pt = SigD (mkName "parse") $ src `arrT` ConT (mkName "Derivs")
	p <- funD (mkName "parse") [mkParseBody th (isJust monad) parsed]
	return [d, pt, p]

derivs :: Bool -> Type -> Type -> Peg -> Dec
derivs th monad src pg = DataD [] (mkName "Derivs") [] [
	RecC (mkName "Derivs") $ map derivs1 pg ++ [
		(mkName dvCharsN, NotStrict, resultT tkn),
		(dvPosN, NotStrict, ConT (mkName "Pos") `AppT` src)
	 ]] []
	where
	tkn = ConT (mkName "Token") `AppT` src
	derivs1 (name, Just t, _) = (mkName name, NotStrict, resultT t)
	derivs1 (name, Nothing, sel) =
		(mkName name, NotStrict, resultT $ getType pg tkn sel)
	resultT typ = ConT (errorTTN th)
		`AppT` (ConT (mkName "ParseError")
			`AppT` (ConT (mkName "Pos") `AppT` src)
			`AppT` ConT (mkName "Derivs"))
		`AppT` monad
		`AppT` (TupleT 2 `AppT` typ `AppT` ConT (mkName "Derivs"))

type Variables = [(String, [Name])]

newVariable :: IORef Int -> Variables -> String -> Q Variables
newVariable g vs n = (: vs) . (n ,) <$> vars
	where vars = runIO $ unsafeInterleaveIO $ (:)
		<$> runQ (newNewName g n) <*> runQ vars

getVariable :: String -> Variables -> Name
getVariable = ((head . fromJust) .) . lookup

nextVariable :: String -> Variables -> Variables
nextVariable n vs = (n, tail $ fromJust $ lookup n vs) : vs

mkParseBody :: Bool -> Bool -> Peg -> ClauseQ
mkParseBody th monadic pg = do
	glb <- runIO $ newIORef 0
	vars <- foldM (newVariable glb) [] [
		"parse", "chars", "pos", "d", "c", "s", "s'", "x", "t", "err", "b",
		"list", "list1", "optional"]
	let	pgn = getVariable "parse" vars
	rets <- mapM (newNewName glb . \(n, _, _) -> n) pg
	rules <- mapM (newNewName glb . \(n, _, _) -> n) pg
	let	decs = flip evalState vars $ (:)
			<$> (FunD pgn <$> (: []) <$> mkParseCore th rets rules)
			<*> zipWithM mkr rules pg
		list = if not $ listUsed pg then [] else listDec
			(getVariable "list" vars) (getVariable "list1" vars) th
		opt = if not $ optionalUsed pg then [] else optionalDec
			(getVariable "optional" vars) th
	return $ Clause [] (NormalB $ -- VarE (runErrorTN th) `AppE`
			VarE pgn `AppE` VarE (mkName "initialPos")) $
		decs ++ list ++ opt
	where
	mkr rule (_, _, sel) =
		flip (ValD $ VarP rule) [] . NormalB <$> mkRule th monadic sel

mkParseCore :: Bool -> [Name] -> [Name] -> State Variables Clause
mkParseCore th rets rules = do
	[ch, p, s, d] <- mapM (gets . getVariable) ["chars", "pos", "s", "d"]
	let def ret rule = flip (ValD $ VarP ret) [] $
		NormalB $ VarE (runStateTN th) `AppE` VarE rule `AppE` VarE d
	pc <- parseChar th
	return $ Clause [VarP p, VarP s] (NormalB $ VarE d) $ [
		flip (ValD $ VarP d) [] $ NormalB $ foldl1 AppE $
			ConE (mkName "Derivs") : map VarE rets ++ [VarE ch, VarE p]
	 ] ++ zipWith def rets rules ++ [pc]

parseChar :: Bool -> State Variables Dec
parseChar th = do
	[prs, ch, p, c, s, s', d] <- mapM (gets . getVariable)
		["parse", "chars", "pos", "c", "s", "s'", "d"]
	let	emsg = "end of input"
		np = VarE (mkName "updatePos") `AppE` VarE c `AppE` VarE p
	return $ flip (ValD $ VarP ch) [] $ NormalB $ VarE (runStateTN th) `AppE`
		CaseE (VarE (mkName "getToken") `AppE` VarE s) [
			Match (mkName "Just" `ConP` [TupP [VarP c, VarP s']])
				(NormalB $ DoE $ map NoBindS [
					VarE (putN th) `AppE`
						(VarE prs `AppE` np
							`AppE` VarE s'),
					VarE (mkName "return") `AppE` VarE c])
				[],
			flip (Match WildP) [] $ NormalB $
				throwErrorTH th (mkName "undefined") [] emsg "" ""
		 ] `AppE` VarE d

mkRule :: Bool -> Bool -> Selection -> State Variables Exp
mkRule t m s = (VarE (mkName "foldl1") `AppE` VarE (mplusN t) `AppE`) . ListE <$>
	case s of
		Left exs -> expression t m `mapM` exs
		Right exs -> zipWithM ((<$>) . lr (length exs)) [0 .. ] $
			map (plainExpression t m) exs
	where
	lr 1 0 ex = ex
	lr _ 0 ex = infixApp (ConE $ mkName "Left") (VarE $ mkName "<$>") ex
	lr l n ex = infixApp (ConE $ mkName "Right") (VarE $ mkName "<$>") $
		lr (l - if n == l - 1 then 1 else 0) (n - 1) ex

expression :: Bool -> Bool -> Expression -> State Variables Exp
expression th m (Left (e, r)) =
	(doE . (++ [NoBindS $ retLift `AppE` r]) . concat <$>) $
		forM e $ \(la, ck@(_, rf, _)) ->
			lookahead th la (show $ pprCheck ck) (readings rf) =<<
				check th m ck
	where
	retLift = if m then VarE $ liftN th else VarE $ mkName "return"
expression th _ (Right e) = do
	c <- gets $ getVariable "c"
	modify $ nextVariable "c"
	let	e' = [(Here, ((VarP c, ""), FromVariable Nothing,
			Just (e `AppE` VarE c, "")))]
		r = VarE c
	expression th False (Left (e', r))

check :: Bool -> Bool -> Check -> State Variables [Stmt]
check th monadic ((n, nc), rf, test) = do
	t <- gets $ getVariable "t"
	d <- gets $ getVariable "d"
	b <- gets $ getVariable "b"
	modify $ nextVariable "t"
	modify $ nextVariable "d"
	modify $ nextVariable "b"
	case (n, test) of
		(WildP, Just p) -> ((BindS (VarP d) (VarE $ getN th) :) .
				(: afterCheck th monadic b p d (readings rf))) .
			BindS WildP <$> transReadFrom th monadic rf
		(_, Just p)
			| notHaveOthers n -> do
				let	bd = BindS (VarP d) $ VarE $ getN th
					m = LetS [ValD n (NormalB $ VarE t) []]
					c = afterCheck th monadic b p d (readings rf)
				s <- BindS (VarP t) <$> transReadFrom th monadic rf
				return $ [bd, s, m] ++ c
			| otherwise -> do
				let	bd = BindS (VarP d) $ VarE $ getN th
					m = beforeMatch th t n d (readings rf) nc
					c = afterCheck th monadic b p d (readings rf)
				s <- BindS (VarP t) <$> transReadFrom th monadic rf
				return $ [bd, s]  ++ m ++ c
		(WildP, _) -> sequence [
			BindS WildP <$> transReadFrom th monadic rf,
			return $ NoBindS $ VarE (mkName "return") `AppE` TupE []
		 ]
		_	| notHaveOthers n ->
				(: []) . BindS n <$> transReadFrom th monadic rf
			| otherwise -> do
				let	bd = BindS (VarP d) $ VarE $ getN th
					m = beforeMatch th t n d (readings rf) nc
				s <- BindS (VarP t) <$> transReadFrom th monadic rf
				return $ bd : s : m
	where
	notHaveOthers (VarP _) = True
	notHaveOthers (TupP pats) = all notHaveOthers pats
	notHaveOthers _ = False

plainExpression :: Bool -> Bool -> PlainExpression -> State Variables Exp
plainExpression th monadic pexs = do
	laxs <- gets $ zip (map fst pexs) . fromJust . lookup "x"
	let mkt = LamE (map (uncurry mkp) laxs) $
		TupE $ map (VarE . snd) $ filter ((== Here) . fst) laxs
	foldl (\x y -> infixApp x (VarE $ mkName "<*>") y)
		(VarE (mkName "return") `AppE` mkt) <$>
			mapM (transHAReadFrom th monadic) pexs
	where
	mkp Here n = VarP n
	mkp _ _ = WildP

transHAReadFrom :: Bool -> Bool -> (Lookahead, ReadFrom) -> State Variables Exp
transHAReadFrom th monadic (ha, rf) = do
	modify $ nextVariable "d"
	doE <$> (lookahead th ha "" (readings rf) =<<
		((: []) . NoBindS <$> transReadFrom th monadic rf))

lookahead :: Bool -> Lookahead -> String -> [String] -> [Stmt] ->
	State Variables [Stmt]
lookahead _ Here _ _ ret = return ret
lookahead th Ahead _ _ ret = do
	d <- gets $ getVariable "d"
	modify $ nextVariable "d"
	return [BindS (VarP d) $ VarE (getN th),
		BindS WildP $ doE ret,
		NoBindS $ VarE (putN th) `AppE` VarE d]
lookahead th (NAhead com) ck ns ret = do
	d <- gets $ getVariable "d"
	modify $ nextVariable "d"
	n <- negative th ('!' : ck) com
		d ns (doE ret)
	return [BindS (VarP d) $ VarE (getN th),
		NoBindS n,
		NoBindS $ VarE (putN th) `AppE` VarE d]

negative :: Bool -> String -> String -> Name -> [String] -> Exp ->
	State Variables Exp
negative th code com d ns act = do
	err <- gets $ getVariable "err"
	modify $ nextVariable "err"
	return $ DoE [
		BindS (VarP err) $ infixApp
			(infixApp act (VarE $ mkName ">>")
				(VarE (mkName "return") `AppE`
					ConE (mkName "False")))
			(VarE $ catchErrorN th)
			(VarE (mkName "const") `AppE`
				(VarE (mkName "return") `AppE`
					ConE (mkName "True"))),
		NoBindS $ VarE (unlessN th) `AppE` VarE err `AppE`
			throwErrorTH th d ns "not match: " code com]

transReadFrom :: Bool -> Bool -> ReadFrom -> State Variables Exp
transReadFrom th _ (FromVariable Nothing) = return $
	ConE (stateTN th) `AppE` VarE (mkName dvCharsN)
transReadFrom th _ (FromVariable (Just var)) = return $
	ConE (stateTN th) `AppE` VarE (mkName var)
transReadFrom th m (FromSelection sel) = mkRule th m sel
transReadFrom th m (FromL List rf) = do
	list <- gets $ getVariable "list"
	(VarE list `AppE`) <$> transReadFrom th m rf
transReadFrom th m (FromL List1 rf) = do
	list1 <- gets $ getVariable "list1"
	(VarE list1 `AppE`) <$> transReadFrom th m rf
transReadFrom th m (FromL Optional rf) = do
	opt <- gets $ getVariable "optional"
	(VarE opt `AppE`) <$> transReadFrom th m rf

beforeMatch :: Bool -> Name -> Pat -> Name -> [String] -> String -> [Stmt]
beforeMatch th t nn d ns nc = [
	NoBindS $ CaseE (VarE t) [
		flip (Match $ vpw nn) [] $ NormalB $
			VarE (mkName "return") `AppE` TupE [],
		flip (Match WildP) [] $ NormalB $
			throwErrorTH th d ns "not match pattern: " (show $ ppr nn) nc],
	LetS [ValD nn (NormalB $ VarE t) []],
	NoBindS $ VarE (mkName "return") `AppE` TupE []]
	where
	vpw (VarP _) = WildP
	vpw (ConP n ps) = ConP n $ map vpw ps
	vpw (InfixP p1 n p2) = InfixP (vpw p1) n (vpw p2)
	vpw (UInfixP p1 n p2) = InfixP (vpw p1) n (vpw p2)
	vpw (ListP ps) = ListP $ vpw `map` ps
	vpw (TupP ps) = TupP $ vpw `map` ps
	vpw o = o

afterCheck :: Bool -> Bool -> Name -> (Exp, String) -> Name -> [String] -> [Stmt]
afterCheck th monadic b (pp, pc) d ns = [
	BindS (VarP b) $ retLift `AppE` pp,
	NoBindS $ VarE (unlessN th) `AppE` VarE b `AppE`
		throwErrorTH th d ns "not match: " (show $ ppr pp) pc]
	where
	retLift = if monadic then VarE $ liftN th else VarE $ mkName "return"

listUsed, optionalUsed :: Peg -> Bool
listUsed = any $ sel . \(_, _, s) -> s
	where
	sel = either (any ex) (any $ any $ rf . snd)
	ex (Left e) = any (rf . (\(_, r, _) -> r) . snd) $ fst e
	ex (Right _) = False
	rf (FromL List _) = True
	rf (FromL List1 _) = True
	rf (FromSelection s) = sel s
	rf _ = False
optionalUsed = any $ sel . \(_, _, s) -> s
	where
	sel = either (any $ ex) (any $ any $ rf . snd)
	ex (Left e) = any (rf . (\(_, r, _) -> r) . snd) $ fst e
	ex (Right _) = False
	rf (FromL Optional _) = True
	rf (FromSelection s) = sel s
	rf _ = False

throwErrorTH :: Bool -> Name -> [String] -> String -> String -> String -> Exp
throwErrorTH th d ns msg code com = InfixE
	(Just $ VarE (getsN th) `AppE` VarE dvPosN)
	(VarE $ mkName ">>=")
	(Just $ InfixE
		(Just $ VarE $ throwErrorN th)
		(VarE $ mkName ".")
		(Just $ VarE (mkName "mkParseError")
			`AppE` LitE (StringL code)
			`AppE` LitE (StringL msg)
			`AppE` LitE (StringL com)
			`AppE` VarE d
			`AppE` ListE (map (LitE . StringL) ns)))

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
showReading d n
	| n == dvCharsN = case flip evalState (0, 0) $ runErrorT $ char d of
		Right (c, _) -> show c
		Left _ -> error "bad"
showReading _ n = "yet: " ++ n

doE :: [Stmt] -> Exp
doE [NoBindS ex] = ex
doE stmts = DoE stmts

arrT :: Type -> Type -> Type
arrT a r = ArrowT `AppT` a `AppT` r

infixApp :: Exp -> Exp -> Exp -> Exp
infixApp e1 op e2 = InfixE (Just e1) op (Just e2)

stateTN, runStateTN, putN, getN, getsN :: Bool -> Name
stateTN True = 'StateT
stateTN False = mkName "StateT"
runStateTN True = 'runStateT
runStateTN False = mkName "runStateT"
putN True = 'put
putN False = mkName "put"
getN True = 'get
getN False = mkName "get"
getsN True = 'gets
getsN False = mkName "gets"

unlessN, mplusN :: Bool -> Name
unlessN True = 'unless
unlessN False = mkName "unless"
mplusN True = 'mplus
mplusN False = mkName "mplus"

throwErrorN, catchErrorN :: Bool -> Name
throwErrorN True = 'throwError
throwErrorN False = mkName "throwError"
catchErrorN True = 'catchError
catchErrorN False = mkName "catchError"

errorTTN, identityN :: Bool -> Name
errorTTN True = ''ErrorT
errorTTN False = mkName "ErrorT"
identityN True = ''Identity
identityN False = mkName "Identity"

liftN :: Bool -> Name
liftN True = 'lift
liftN False = mkName "lift"

getType :: Peg -> Type -> Selection -> Type
getType pg tkn s = case s of
	Right e -> foldr1 (\x y -> (ConT (mkName "Either") `AppT` x) `AppT` y) $
		map (mkt . map (rf . snd) . filter ((== Here) . fst)) e
	Left [Right _] -> tkn
	Left [Left ([(Here, ((VarP p, _), FromVariable Nothing, _))], VarE v)]
		| p == v -> tkn
	_ -> error "getType: can't get type"
	where
	rf (FromVariable (Just v)) = let
		def = case flip filter pg $ (== v) . \(n, _, _) -> n of
			[d] -> d
			_ -> error "search: bad" in
		case def of
			(_, Just typ, _) -> typ
			(_, _, sel) -> getType pg tkn sel
	rf (FromVariable _) = tkn
	rf (FromSelection sel) = getType pg tkn sel
	rf (FromL Optional r) = ConT (mkName "Maybe") `AppT` rf r
	rf (FromL _ r) = ListT `AppT` rf r
	mkt ts = foldl AppT (TupleT $ length ts) ts

readings :: ReadFrom -> [String]
readings (FromVariable (Just s)) = [s]
readings (FromVariable _) = [dvCharsN]
readings (FromL _ rf) = readings rf
readings (FromSelection s) = concat $ either
	(mapM $ either
		(readings . (\(_, rf, _) -> rf) . snd . head . fst)
		(const [dvCharsN]))
	(mapM $ concatMap $ readings . snd) s
