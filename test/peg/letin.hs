{-# LANGUAGE TypeFamilies, QuasiQuotes #-}

import Text.Papillon

parseHoge :: String -> Maybe (Maybe String)
parseHoge src
	| Right (c, _) <- runError $ hoge $ parse src = Just c
	| otherwise = Nothing

[papillon|

hoge :: Maybe String
	= c		{ if False then Just "" else let a = Just "oge" in
				a }

|]
