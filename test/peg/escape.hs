{-# LANGUAGE TypeFamilies, QuasiQuotes #-}

import Text.Papillon

parseSpace :: String -> Maybe Char
parseSpace src
	| Right (c, _) <- runError $ hogespace $ hogeparse src = Just c
	| otherwise = Nothing

[papillon|

prefix:"hoge"

space :: Char
	= '\x0020'	{ '\x0020' }
	/ '\x3042'	{ '\x3042' }

|]
