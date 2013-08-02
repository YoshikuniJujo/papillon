module Text.Papillon (
	papillon,
	Source(..),
	SourceList(..),
	Pos(..),
	ListPos(..),

	-- * For parse error message
	ParseError,
	mkParseError,
	peDerivs,
	peReading,
	peMessage,
	peCode,
	peComment,
	pePosition,
	pePositionS,
	(<*>),
	(<$>),
	runError
) where

import Text.Papillon.Core
import Language.Haskell.TH.Quote

papillon :: QuasiQuoter
papillon = QuasiQuoter {
	quoteExp = undefined,
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = papillonCore
 }
