{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import Text.Papillon

run :: String -> Maybe String
run s = case runError $ palindrome $ parse s of
	Right (r, _) -> Just r
	_ -> Nothing

[papillon|

palindrome :: String
	= o:op !_		{ o }
	/ e:ep !_		{ e }

ep :: String
	= c p:ep c':[c == c']	{ c : p }
	/			{ "" }

op :: String
	= c p:op c':[c == c']	{ c : p }
	/ c			{ [c] }

|]
