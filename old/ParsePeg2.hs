{-# LANGUAGE PackageImports #-}

module ParsePeg2 (
) where

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.State
import Data.Char

type Result v = Maybe (v, Derivs)

data One = Charlit Char | Variable String deriving Show

data Derivs = Derivs {
	dvPeg :: Result [(String, [[One]])],
	dvDefinition :: Result (String, [[One]]),
	dvSelection :: Result [[One]],
	dvExpression :: Result [One],
	dvOne :: Result One,
	dvVariable :: Result String,
	dvCharlit :: Result Char,
	dvSmall :: Result Char,
	dvSymbol1 :: Result Char,
	dvSymbol2 :: Result Char,
	dvSpaces :: Result (),
	dvSpace :: Result (),
	dvChar :: Result Char
 } deriving Show

type PMonad = StateT Derivs Maybe

dvPegM :: PMonad [(String, [[One]])]
dvPegM = StateT dvPeg

dvDefinitionM :: PMonad (String, [[One]])
dvDefinitionM = StateT dvDefinition

dvSelectionM :: PMonad [[One]]
dvSelectionM = StateT dvSelection

dvExpressionM :: PMonad [One]
dvExpressionM = StateT dvExpression

dvOneM :: PMonad One
dvOneM = StateT dvOne

dvVariableM :: PMonad String
dvVariableM = StateT dvVariable

dvCharlitM, dvSmallM, dvSymbol1M, dvSymbol2M :: PMonad Char
dvCharlitM = StateT dvCharlit
dvSmallM = StateT dvSmall
dvSymbol1M = StateT dvSymbol1
dvSymbol2M = StateT dvSymbol2

dvSpacesM, dvSpaceM :: PMonad ()
dvSpacesM = StateT dvSpaces
dvSpaceM = StateT dvSpace

dvCharM :: PMonad Char
dvCharM = StateT dvChar

parse :: String -> Derivs
parse s = d where
	d = Derivs
		peg definition selection expression one variable charlit small
		symbol1 symbol2 spaces space chr
	peg = runStateT pPeg d
	definition = runStateT pDefinition d
	selection = runStateT pSelection d
	expression = runStateT pExpression d
	one = runStateT pOne d
	variable = runStateT pVariable d
	charlit = runStateT pCharlit d
	small = runStateT pSmall d
	symbol1 = runStateT pSymbol1 d
	symbol2 = runStateT pSymbol2 d
	spaces = runStateT pSpaces d
	space = runStateT pSpace d
	chr = flip runStateT d $ do
		c : s' <- return s
		put $ parse s'
		return c

pPeg :: PMonad [(String, [[One]])]
pPeg = msum [
	(\d _ p -> d : p) <$> dvDefinitionM <*> dvSpacesM <*> dvPegM, return []]

pDefinition :: PMonad (String, [[One]])
pDefinition = (\v _ _ _ s _ -> (v, s))
	<$> dvVariableM
	<*> dvSpacesM
	<*> do { '=' <- dvCharM; return () }
	<*> dvSpacesM
	<*> dvSelectionM
	<*> do { ';' <- dvCharM; return () }

pSelection :: PMonad [[One]]
pSelection = msum [
	(\e _ _ _ s -> e : s)
		<$> dvExpressionM
		<*> dvSpacesM
		<*> do { '/' <- dvCharM; return () }
		<*> dvSpacesM
		<*> dvSelectionM,
	(: []) <$> dvExpressionM]

pExpression :: PMonad [One]
pExpression = msum [
	(\o _ e -> o : e) <$> dvOneM <*> dvSpacesM <*> dvExpressionM,
	return []]

pOne :: PMonad One
pOne = msum [Variable <$> dvVariableM, Charlit <$> dvCharlitM]

pVariable :: PMonad String
pVariable = msum [(:) <$> dvSmallM <*> dvVariableM, (: []) <$> dvSmallM]

pCharlit, pSmall, pSymbol1, pSymbol2 :: PMonad Char
pCharlit = do
	'\'' <- dvCharM
	c <- msum [dvSmallM, dvSymbol1M, do { '\\' <- dvCharM; dvSymbol2M }]
	'\'' <- dvCharM
	return c
pSmall = do
	c <- dvCharM
	if isLower c then return c else fail "not match"
pSymbol1 = do
	c <- dvCharM
	if c `elem` "=/;+*() " then return c else fail "not match"
pSymbol2 = do
	c <- dvCharM
	if c `elem` "\\'" then return c else case c of
		'n' -> return '\n'
		't' -> return '\t'
		_ -> fail "not match"

pSpaces :: PMonad ()
pSpaces = (dvSpaceM >> dvSpacesM) `mplus` return ()

pSpace :: PMonad ()
pSpace = do
	c <- dvCharM
	unless (isSpace c) $ fail "not match"
