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
) where

import Text.PapillonCore
import Language.Haskell.TH.Quote

papillon :: QuasiQuoter
papillon = QuasiQuoter {
	quoteExp = undefined,
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = papillonCore
 }
