{-# LANGUAGE PackageImports #-}

module ParsePeg (
) where

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.State

type Result v = Maybe (v, Derivs)

data Peg
	= Peg1 Definition Spaces Peg
	| Peg2
	deriving Show
data Definition
	= Definition1 Variable Spaces Spaces Selection
	deriving Show
data Selection
	= Selection1 Expression Spaces Spaces Expression
	| Selection2 Expression
	deriving Show
data Expression
	= Expression1 One Spaces Expression
	| Expression2
	deriving Show
data One
	= One1 Variable
	| One2 Charlit
	deriving Show
data Variable
	= Variable1 Small Variable
	| Variable2 Small
	deriving Show
data Charlit
	= Charlit1 Char
	| Charlit2 Char
	| Charlit3 Char
	| Charlit4 Char
	deriving Show
data Small
	= Small1  | Small2  | Small3  | Small4  | Small5  | Small6  | Small7
	| Small8  | Small9  | Small10 | Small11 | Small12 | Small13 | Small14
	| Small15 | Small16 | Small17 | Small18 | Small19 | Small20 | Small21
	| Small22 | Small23 | Small24 | Small25 | Small26
	deriving Show
data Digit
	= Digit1 | Digit2 | Digit3 | Digit5 | Digit6 | Digit7 | Digit8 | Digit9
	| Digit10
	deriving Show
data Symbol1
	= Symbol11 | Symbol12 | Symbol13 | Symbol14 | Symbol15 | Symbol16
	| Symbol17 | Symbol18
	deriving Show
data Symbol2
	= Symbol21 | Symbol22 | Symbol23 | Symbol24
	deriving Show
data Spaces
	= Spaces1 Char Spaces
	| Spaces2 Char
	deriving Show
data Space
	= Space1 | Space2 | Space3
	deriving (Show, Enum)

data Derivs = Derivs {
	dvPeg :: Result Peg,
	dvDefinition :: Result Definition,
	dvSelection :: Result Selection,
	dvExpression :: Result Expression,
	dvOne :: Result One,
	dvVariable :: Result Variable,
	dvCharlit :: Result Charlit,
	dvSmall :: Result Char,
	dvDigit :: Result Char,
	dvSymbol1 :: Result Char,
	dvSymbol2 :: Result Char,
	dvSpaces :: Result Spaces,
	dvSpace :: Result Char,
	dvChar :: Result Char
} deriving Show

dvPegM = StateT dvPeg
dvDefinitionM = StateT dvDefinition
dvSelectionM = StateT dvSelection
dvExpressionM = StateT dvExpression
dvOneM = StateT dvOne
dvVariableM = StateT dvVariable
dvCharlitM = StateT dvCharlit
dvSmallM = StateT dvSmall
dvDigitM = StateT dvDigit
dvSymbol1M = StateT dvSymbol1
dvSymbol2M = StateT dvSymbol2
dvSpacesM = StateT dvSpaces
dvSpaceM = StateT dvSpace
dvCharM = StateT dvChar

parse :: String -> Derivs
parse s = d where
	d = Derivs
		undefined undefined undefined undefined undefined undefined
		charlit small digit symbol1 symbol2 spaces
		space char
	charlit = runStateT pCharlit d
	small = runStateT pSmall d
	digit = runStateT pDigit d
	symbol1 = runStateT pSymbol1 d
	symbol2 = runStateT pSymbol2 d
	spaces = runStateT pSpaces d
	space = runStateT pSpace d
	char = flip runStateT d $ do
		c : s' <- return s
		put $ parse s'
		return c

charM :: Char -> StateT Derivs Maybe Char
charM c0 = do
	c <- dvCharM
	if c == c0 then return c else fail "not match"

pCharlit :: StateT Derivs Maybe Charlit
pCharlit = msum [alt1, alt2, alt3, alt4] where
	alt1 = do
		'\'' <- dvCharM
		v1 <- dvSmallM
		'\'' <- dvCharM
		return $ Charlit1 v1
	alt2 = do
		'\'' <- dvCharM
		v1 <- dvDigitM
		'\'' <- dvCharM
		return $ Charlit2 v1
	alt3 = do
		'\'' <- dvCharM
		v1 <- dvSymbol1M
		'\'' <- dvCharM
		return $ Charlit3 v1
	alt4 = do
		'\'' <- dvCharM
		'\\' <- dvCharM
		v1 <- dvSymbol2M
		'\'' <- dvCharM
		return $ Charlit4 v1
	
pSmall, pDigit :: StateT Derivs Maybe Char
pSmall = msum $ map charM ['a' .. 'z']
pDigit = msum $ map charM "0123456789"

pSymbol1 :: StateT Derivs Maybe Char
pSymbol1 = msum [alt1, alt2, alt3, alt4, alt5, alt6, alt7, alt8]
	where
	alt1 = do
		'=' <- dvCharM
		return '='
	alt2 = do
		'/' <- dvCharM
		return '/'
	alt3 = do
		';' <- dvCharM
		return ';'
	alt4 = do
		'+' <- dvCharM
		return '+'
	alt5 = do
		'*' <- dvCharM
		return '*'
	alt6 = do
		'(' <- dvCharM
		return '('
	alt7 = do
		')' <- dvCharM
		return ')'
	alt8 = do
		' ' <- dvCharM
		return ' '

pSymbol2 :: StateT Derivs Maybe Char
pSymbol2 = alt1 `mplus` alt2 `mplus` alt3 `mplus` alt4 where
	alt1 = do
		'\\' <- dvCharM
		return '\\'
	alt2 = do
		'\'' <- dvCharM
		return '\''
	alt3 = do
		'n' <- dvCharM
		return 'n'
	alt4 = do
		't' <- dvCharM
		return 't'

pSpaces :: StateT Derivs Maybe Spaces
pSpaces = alt1 `mplus` alt2 where
	alt1 = do
		v1 <- dvSpaceM
		v2 <- dvSpacesM
		return $ Spaces1 v1 v2
	alt2 = do
		v1 <- dvSpaceM
		return $ Spaces2 v1

pSpace :: StateT Derivs Maybe Char
pSpace = alt1 `mplus` alt2 `mplus` alt3 where
	alt1 = do
		' ' <- dvCharM
		return ' '
	alt2 = do
		'\n' <- dvCharM
		return '\n'
	alt3 = do
		'\t' <- dvCharM
		return '\t'
