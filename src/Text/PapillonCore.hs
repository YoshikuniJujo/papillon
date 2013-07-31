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
	(<*>),
	(<$>)
) where

import Language.Haskell.TH hiding (infixApp, doE)
import "monads-tf" Control.Monad.State
-- import "monads-tf" Control.Monad.Identity
import "monads-tf" Control.Monad.Error

import Control.Applicative

import Text.Papillon.Parser
import Data.Maybe
import Data.IORef

import Text.Papillon.List

import System.IO.Unsafe

dvCharsN, dvPosN :: Name
dvCharsN = mkName "char"
dvPosN = mkName "position"

papillonCore :: String -> DecsQ
papillonCore str = case peg $ parse str of
	Right (stpegq, _) -> do
		(src, parsed) <- stpegq =<< runIO (newIORef 0)
		decParsed True src parsed
	Left err -> error $ "parse error: " ++ showParseError err

papillonFile :: String ->
	Q ([PPragma], ModuleName, Maybe Exports, Code, DecsQ, Code)
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

type Variables = [(String, [Name])]

newVariable :: IORef Int -> Variables -> String -> Q Variables
newVariable g vs n = (: vs) . (n ,) <$> vars
	where vars = runIO $ unsafeInterleaveIO $ (:)
		<$> runQ (newNewName g n) <*> runQ vars

getVariable :: String -> Variables -> Name
getVariable = ((head . fromJust) .) . lookup

nextVariable :: String -> Variables -> Variables
nextVariable n vs = (n, tail $ fromJust $ lookup n vs) : vs

mkParseBody :: Bool -> Peg -> ClauseQ
mkParseBody th pg = do
	glb <- runIO $ newIORef 0
	vars <- foldM (newVariable glb) [] [
		"parse", "chars", "pos", "d", "c", "s", "s'", "x", "t",
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
	return $ Clause [] (NormalB $ VarE pgn `AppE` VarE (mkName "initialPos")) $
		decs ++ list ++ opt
	where
	mkr rule (_, _, sel) =
		flip (ValD $ VarP rule) [] . NormalB <$> mkRule th sel

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
				newThrow th "" emsg (mkName "undefined") [] ""
		 ] `AppE` VarE d

mkRule :: Bool -> Selection -> State Variables Exp
mkRule t s = (VarE (mkName "foldl1") `AppE` VarE (mplusN t) `AppE`) . ListE <$>
	case s of
		Left exs -> expression t `mapM` exs
		Right exs -> zipWithM ((<$>) . lr (length exs)) [0 .. ] $
			map (plainExpression t) exs
	where
	lr 1 0 ex = ex
	lr _ 0 ex = infixApp (ConE $ mkName "Left") (VarE $ mkName "<$>") ex
	lr l n ex = infixApp (ConE $ mkName "Right") (VarE $ mkName "<$>") $
		lr (l - if n == l - 1 then 1 else 0) (n - 1) ex

expression :: Bool -> Expression -> State Variables Exp
expression th (e, r) =
	(doE . (++ [NoBindS $ VarE (mkName "return") `AppE` r]) . concat <$>) $
		forM e $ \(la, ck@(_, rf, _)) ->
			lookahead th la (show $ pprCheck ck) (nameFromRF rf) =<<
				transLeaf th ck

transLeaf :: Bool -> Check -> State Variables [Stmt]
transLeaf th ((n, nc), rf, Just (p, pc)) = do
	t <- gets $ getVariable "t"
	d <- gets $ getVariable "d"
	modify $ nextVariable "t"
	modify $ nextVariable "d"
	case n of
		WildP -> ((BindS (VarP d) (VarE $ getN th) :) .
				(: [afterCheck th p d (nameFromRF rf) pc])) .
			BindS WildP <$> transReadFrom th rf
		_	| notHaveOthers n -> do
				let	bd = BindS (VarP d) $ VarE $ getN th
					m = LetS [ValD n (NormalB $ VarE t) []]
					c = afterCheck th p d (nameFromRF rf) pc
				s <- BindS (VarP t) <$> transReadFrom th rf
				return [bd, s, m, c]
			| otherwise -> do
				let	bd = BindS (VarP d) $ VarE $ getN th
					m = beforeMatch th t n d (nameFromRF rf) nc
					c = afterCheck th p d (nameFromRF rf) pc
				s <- BindS (VarP t) <$> transReadFrom th rf
				return $ [bd, s]  ++ m ++ [c]
	where
	notHaveOthers (VarP _) = True
	notHaveOthers (TupP pats) = all notHaveOthers pats
	notHaveOthers _ = False
transLeaf th ((n, nc), rf, Nothing) = do
	t <- gets $ getVariable "t"
	d <- gets $ getVariable "d"
	modify $ nextVariable "d"
	modify $ nextVariable "t"
	case n of
		WildP -> sequence [
			BindS WildP <$> transReadFrom th rf,
			return $ NoBindS $ VarE (mkName "return") `AppE` TupE []
		 ]
		_	| notHaveOthers n ->
				(: []) . BindS n <$> transReadFrom th rf
			| otherwise -> do
				let	bd = BindS (VarP d) $ VarE $ getN th
					m = beforeMatch th t n d (nameFromRF rf) nc
				s <- BindS (VarP t) <$> transReadFrom th rf
				return $ bd : s : m
	where
	notHaveOthers (VarP _) = True
	notHaveOthers (TupP pats) = all notHaveOthers pats
	notHaveOthers _ = False

plainExpression :: Bool -> PlainExpression -> State Variables Exp
plainExpression th pexs = do
	laxs <- gets $ zip (map fst pexs) . fromJust . lookup "x"
	let mkt = LamE (map (uncurry mkp) laxs) $
		TupE $ map (VarE . snd) $ filter ((== Here) . fst) laxs
	foldl (\x y -> infixApp x (VarE $ mkName "<*>") y)
		(VarE (mkName "return") `AppE` mkt) <$>
			mapM (transHAReadFrom th) pexs
	where
	mkp Here n = VarP n
	mkp _ _ = WildP

transHAReadFrom :: Bool -> (Lookahead, ReadFrom) -> State Variables Exp
transHAReadFrom th (ha, rf) = do
	modify $ nextVariable "d"
	doE <$> (lookahead th ha "" (nameFromRF rf) =<<
		((: []) . NoBindS <$> transReadFrom th rf))

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
	return [BindS (VarP d) $ VarE (getN th),
		NoBindS $ negative th
			(LitE $ StringL ck)
			(LitE $ StringL com)
			(VarE d)
			(ListE $ map (LitE . StringL) ns)
			(doE ret),
		NoBindS $ VarE (putN th) `AppE` VarE d]

negative :: Bool -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp
negative th code com d ns act = DoE [
	BindS (VarP $ mkName "err") $ InfixE
		(Just actionReturnFalse)
		(VarE $ catchErrorN th)
		(Just constReturnTrue),
	NoBindS $ VarE (unlessN th)
		`AppE` VarE (mkName "err")
		`AppE` throwErrorPackratMBody th
			(InfixE (Just $ LitE $ CharL '!') (ConE $ mkName ":")
				(Just code))
			(LitE $ StringL "not match: ") com d ns
 ]	where
	actionReturnFalse = InfixE (Just act) (VarE $ mkName ">>")
		(Just $ VarE (mkName "return") `AppE` ConE (mkName "False"))
	constReturnTrue = VarE (mkName "const") `AppE` 
		(VarE (mkName "return") `AppE` ConE (mkName "True"))

transReadFrom :: Bool -> ReadFrom -> State Variables Exp
transReadFrom th (FromVariable Nothing) = return $
	ConE (stateTN th) `AppE` VarE dvCharsN
transReadFrom th (FromVariable (Just var)) = return $
	ConE (stateTN th) `AppE` VarE (mkName var)
transReadFrom th (FromSelection sel) = mkRule th sel
transReadFrom th (FromL List rf) = do
	list <- gets $ getVariable "list"
	(VarE list `AppE`) <$> transReadFrom th rf
transReadFrom th (FromL List1 rf) = do
	list1 <- gets $ getVariable "list1"
	(VarE list1 `AppE`) <$> transReadFrom th rf
transReadFrom th (FromL Optional rf) = do
	opt <- gets $ getVariable "optional"
	(VarE opt `AppE`) <$> transReadFrom th rf

beforeMatch :: Bool -> Name -> Pat -> Name -> [String] -> String -> [Stmt]
beforeMatch th t nn d ns nc = [
	NoBindS $ CaseE (VarE t) [
		flip (Match $ vpw nn) [] $ NormalB $
			VarE (mkName "return") `AppE` TupE [],
		flip (Match WildP) [] $ NormalB $
			newThrow th (show $ ppr nn) "not match pattern: " d ns nc],
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

afterCheck :: Bool -> Exp -> Name -> [String] -> String -> Stmt
afterCheck th pp d ns pc = NoBindS $ VarE (unlessN th) `AppE` pp `AppE`
	newThrow th (show $ ppr pp) "not match: " d ns pc

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

newThrow :: Bool -> String -> String -> Name -> [String] -> String -> Exp
newThrow th code msg d ns com =
	throwErrorPackratMBody th
		(LitE $ StringL code)
		(LitE $ StringL msg)
		(LitE $ StringL com)
		(VarE d)
		(ListE $ map (LitE . StringL) ns)

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
