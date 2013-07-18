module Text.Papillon (
	papillon,
	ParseError,
	mkParseError,
	peCode,
	peMessage,
	peDerivs,
	peComment,
	peReading,
	pePosition,
	pePositionS,
	Source(..),
	SourceList(..),
	Pos(..),
	ListPos(..),
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
