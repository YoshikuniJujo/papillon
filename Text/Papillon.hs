module Text.Papillon (
	papillon
) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH

papillon :: QuasiQuoter
papillon = QuasiQuoter {
	quoteExp = undefined,
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = declaration
 }

declaration :: String -> DecsQ
declaration src = do
	parseT <- sigD (mkName "parse") $
		arrowT `appT` conT (mkName "String") `appT`
			(conT (mkName "Maybe") `appT`
				(tupleT 2
					`appT` conT (mkName "Int")
					`appT` conT (mkName "Int")))
	parse <- funD (mkName "parse") [c] -- valD (varP $ mkName "parse") (normalB $ litE $ integerL 8) []
	dvSome <- valD (varP $ mkName "dvSome") (normalB $ varE $ mkName "id") []
	return [parseT, parse, dvSome]
	where
	c = clause [wildP] (normalB $ conE $ mkName "Nothing") []
