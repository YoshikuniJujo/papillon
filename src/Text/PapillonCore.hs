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
	ExportList,
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

isOptionalUsed :: Peg -> Bool
isOptionalUsed = any isOptionalUsedDefinition

isOptionalUsedDefinition :: Definition -> Bool
isOptionalUsedDefinition (_, _, Left sel) = any isOptionalUsedSelection sel
isOptionalUsedDefinition (_, _, Right sel) = any isOptionalUsedPlainSelection sel

isOptionalUsedSelection :: Expression -> Bool
isOptionalUsedSelection (_, exhs) = let (ex, _) = exhs $ mkName "c" in
	any isOptionalUsedLeafName ex

isOptionalUsedPlainSelection :: PlainExpression -> Bool
isOptionalUsedPlainSelection rfs = any (isOptionalUsedReadFrom . snd) rfs

isOptionalUsedLeafName :: (HA, NameLeaf) -> Bool
isOptionalUsedLeafName (_, nl) = isOptionalUsedLeafName' nl

isOptionalUsedLeafName' :: NameLeaf -> Bool
isOptionalUsedLeafName' (_, rf, _) = isOptionalUsedReadFrom rf

isOptionalUsedReadFrom :: ReadFrom -> Bool
isOptionalUsedReadFrom (FromL Optional _) = True
isOptionalUsedReadFrom (FromSelection (Left sel)) =
	any isOptionalUsedSelection sel
isOptionalUsedReadFrom _ = False

isListUsed :: Peg -> Bool
isListUsed = any isListUsedDefinition

isListUsedDefinition :: Definition -> Bool
isListUsedDefinition (_, _, Left sel) = any isListUsedSelection sel
isListUsedDefinition (_, _, Right sel) = any isListUsedPlainSelection sel

isListUsedSelection :: Expression -> Bool
isListUsedSelection (_, exhs) = let (ex, _) = exhs $ mkName "c" in
	any isListUsedLeafName ex

isListUsedPlainSelection :: PlainExpression -> Bool
isListUsedPlainSelection rfs = any (isListUsedReadFrom . snd) rfs

isListUsedLeafName :: (HA, NameLeaf) -> Bool
isListUsedLeafName (_, nl) = isListUsedLeafName' nl

isListUsedLeafName' :: NameLeaf -> Bool
isListUsedLeafName' (_, (FromL List _), _) = True
isListUsedLeafName' (_, (FromL List1 _), _) = True
isListUsedLeafName' _ = False

isListUsedReadFrom :: ReadFrom -> Bool
isListUsedReadFrom (FromL List _) = True
isListUsedReadFrom (FromL List1 _) = True
isListUsedReadFrom _ = False

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

eitherN :: Name
eitherN = mkName "Either"

papillonCore :: String -> DecsQ
papillonCore str = case peg $ parse str of
	Right ((src, parsed), _) ->
		decParsed True src (conT (mkName "Token") `appT` src) parsed
	Left err -> error $ "parse error: " ++ showParseError err

papillonFile :: String ->
	([PPragma], ModuleName, Maybe ExportList, Code, DecsQ, Code)
papillonFile str = case pegFile $ parse str of
	Right ((prgm, mn, ppp, pp, (src, parsed), atp), _) ->
		(prgm, mn, ppp, addApplicative parsed ++ pp,
		decParsed False src (conT (mkName "Token") `appT` src) parsed, atp)
	Left err -> error $ "parse error: " ++ showParseError err
	where
	addApplicative pg = if needApplicative pg
		then "import Control.Applicative\n"
		else ""
	needApplicative pg = isListUsed pg || isOptionalUsed pg

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

decParsed :: Bool -> TypeQ -> TypeQ -> Peg -> DecsQ
decParsed th src tkn parsed = do
	glb <- runIO $ newIORef 0
	d <- derivs th src tkn parsed
	pt <- parseT src th
	p <- funD (mkName "parse") [parseEE glb th parsed]
	return $ d : pt : [p]

parseEE :: IORef Int -> Bool -> Peg -> ClauseQ
parseEE glb th pg = do
	pgn <- newNewName glb "parse"
	listN <- newNewName glb "list"
	list1N <- newNewName glb "list1"
	optionalN <- newNewName glb "optional"
	pNames <- mapM (newNewName glb . getDefinitionName) pg

	pgenE <- varE pgn `appE` varE (mkName "initialPos")
	decs <- (:)
		<$> funD pgn [parseE glb th pgn pNames pg]
		<*> pSomes glb th listN list1N optionalN pNames pg
	ld <- listDec listN list1N th
	od <- optionalDec optionalN th
	return $ Clause [] (NormalB pgenE) $ decs ++
		(if isListUsed pg then ld else []) ++
		(if isOptionalUsed pg then od else [])

dvCharsN, dvPosN :: Name
dvCharsN = mkName "char"
dvPosN = mkName "position"

derivs :: Bool -> TypeQ -> TypeQ -> Peg -> DecQ
derivs _ src tkn pegg = dataD (cxt []) (mkName "Derivs") [] [
	recC (mkName "Derivs") $ map (derivs1 pegg src tkn) pegg ++ [
		varStrictType dvCharsN $ strictType notStrict $
			resultT src tkn,
		varStrictType dvPosN $ strictType notStrict $
			conT (mkName "Pos") `appT` src
	 ]
 ] []

derivs1 :: Peg -> TypeQ -> TypeQ -> Definition -> VarStrictTypeQ
derivs1 _ src _ (name, Just typ, _) =
	varStrictType (mkName name) $ strictType notStrict $ resultT src typ
derivs1 pg src tkn (name, _, sel) =
	varStrictType (mkName name) $ strictType notStrict $ resultT src $
		getSelectionType pg tkn sel

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

resultT :: TypeQ -> TypeQ -> TypeQ
resultT src typ =
	conT eitherN `appT` pe `appT`
		(tupleT 2 `appT` typ `appT` conT (mkName "Derivs"))
	where
	pe = conT (mkName "ParseError")
		`appT` (conT (mkName "Pos") `appT` src)
		`appT` conT (mkName "Derivs")

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
	parseE' g th pgn tmps pnames $ map mkName names
	where
	names = map getDefinitionName pegg

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
pSomes g th lst lst1 opt = zipWithM $ pSomes1 g th lst lst1 opt

pSomes1 :: IORef Int -> Bool -> Name -> Name -> Name -> Name -> Definition -> DecQ
pSomes1 g th lst lst1 opt pname (_, _, sel) =
	flip (valD $ varP pname) [] $ normalB $ pSomes1Sel g th lst lst1 opt sel

pSomes1Sel :: IORef Int -> Bool -> Name -> Name -> Name -> Selection -> ExpQ
pSomes1Sel g th lst lst1 opt (Left sel) =
	varE (mkName "foldl1") `appE` varE (mplusN th) `appE`
		listE (map (processExpressionHs g th lst lst1 opt) sel)
pSomes1Sel g th lst lst1 opt (Right sel) =
	varE (mkName "foldl1") `appE` varE (mplusN th) `appE`
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
processExpressionHs g th lst lst1 opt (_, exhs) = do
	c <- newNewName g "c"
	let (expr, ret) = exhs c
	fmap smartDoE $ do
		x <- forM expr $ \(ha, nl@(_, rf, _)) -> do
			nls <- showNameLeaf nl
			processHA g th ha nls (nameFromRF rf) $
				transLeaf g th lst lst1 opt nl
		r <- noBindS $ varE (returnN th) `appE` ret
		return $ concat x ++ [r]

processPlainExpressionHs ::
	IORef Int -> Bool -> Name -> Name -> Name -> PlainExpression -> ExpQ
processPlainExpressionHs g th lst lst1 opt rfs =
	foldl (\x y -> infixApp x appApply y)
		(returnEQ `appE` mkTupleE g (map fst rfs)) $
			map (transHAReadFrom g th lst lst1 opt) $ rfs

mkTupleE :: IORef Int -> [HA] -> ExpQ
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

transHAReadFrom ::
	IORef Int -> Bool -> Name -> Name -> Name -> (HA, ReadFrom) -> ExpQ
transHAReadFrom g th lst lst1 opt (ha, rf) = smartDoE <$>
	(processHA g th ha "" (nameFromRF rf) $ fmap (: []) $ noBindS $
		transReadFrom g th lst lst1 opt rf)

transReadFrom :: IORef Int -> Bool -> Name -> Name -> Name -> ReadFrom -> ExpQ
transReadFrom _ th _ _ _ (FromVariable Nothing) =
	conE (stateTN' th) `appE` varE dvCharsN
transReadFrom _ th _ _ _ (FromVariable (Just var)) =
	conE (stateTN' th) `appE` varE (mkName var)
transReadFrom g th l l1 o (FromSelection sel) = pSomes1Sel g th l l1 o sel
transReadFrom g th l l1 o (FromL List rf) = varE l `appE` transReadFrom g th l l1 o rf
transReadFrom g th l l1 o (FromL List1 rf) = varE l1 `appE` transReadFrom g th l l1 o rf
transReadFrom g th l l1 o (FromL Optional rf) = varE o `appE` transReadFrom g th l l1 o rf

mkTDNN :: IORef Int -> PatQ -> Q (Name, Name, Pat)
mkTDNN g n = do
	t <- getNewName g "xx"
	d <- getNewName g "d"
	nn <- n
	return (t, d, nn)

transLeaf :: IORef Int -> Bool -> Name -> Name -> Name -> NameLeaf -> Q [Stmt]
transLeaf g th lst lst1 opt ((n, nc), rf, Just (p, pc)) = do
	(t, d, nn) <- mkTDNN g n
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
				s <- bindS (varP t) $
						transReadFrom g th lst lst1 opt rf
				m <- beforeMatch th t n d (nameFromRF rf) nc
				c <- afterCheck th p d (nameFromRF rf) pc
				return $ bd : s : m ++ [c]
	where
	notHaveOthers (VarP _) = True
	notHaveOthers (TupP pats) = all notHaveOthers pats
	notHaveOthers _ = False
transLeaf g th lst lst1 opt ((n, nc), rf, Nothing) = do
	(t, d, nn) <- mkTDNN g n
	case nn of
		WildP -> sequence [
			bindS wildP $ transReadFrom g th lst lst1 opt rf,
			noBindS $ varE (returnN th) `appE` tupE []
		 ]
		_	| notHaveOthers nn -> (: []) <$>
				bindS n (transReadFrom g th lst lst1 opt rf)
			| otherwise -> do
				bd <- bindS (varP d) $ varE $ getN th
				s <- bindS (varP t) $
					transReadFrom g th lst lst1 opt rf
				m <- beforeMatch th t n d (nameFromRF rf) nc
				return $ bd : s : m
	where
	notHaveOthers (VarP _) = True
	notHaveOthers (TupP pats) = all notHaveOthers pats
	notHaveOthers _ = False

processHA :: IORef Int -> Bool -> HA -> String -> [String] -> Q [Stmt] -> Q [Stmt]
processHA _ _ Here _ _ act = act
processHA g th After _ _ act = do
	d <- getNewName g "ddd"
	sequence [
		bindS (varP d) $ varE (getN th),
		bindS wildP $ smartDoE <$> act,
		noBindS $ varE (putN th) `appE` varE d]
processHA g th (NotAfter com) nls names act = do
	d <- getNewName g "ddd"
	sequence [
		bindS (varP d) $ varE (getN th),
		noBindS $ flipMaybeBody th
			(stringE nls)
			(stringE com)
			(varE d)
			(listE $ map stringE names)
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
