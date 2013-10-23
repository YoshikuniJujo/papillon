{-# LANGUAGE TypeFamilies, QuasiQuotes #-}

import Text.Papillon

parseHoge :: String -> Maybe String
parseHoge src
	| Right (c, _) <- runError $ hoge $ parse src = Just c
	| otherwise = Nothing

[papillon|

hoge :: String
	= c		{ if c == 'h' then "hoge" else "piyo" }

|]
