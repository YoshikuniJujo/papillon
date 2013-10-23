{-# LANGUAGE TypeFamilies, QuasiQuotes #-}

import Text.Papillon

parseAs :: String -> Maybe String
parseAs src
	| Right (c, _) <- runError $ as $ parse src = Just c
	| otherwise = Nothing

[papillon|

as :: String
	= (a@(h:oge)):hoge	{ a ++ oge }

hoge :: String
	= 'h'		{ "hoge" }

|]
