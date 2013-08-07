{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import Text.Papillon

run :: String -> Maybe [Door]
run s = case runError $ start $ parse $ map ctd s of
	Right (r, d) -> case runError $ char d of
		Left _ -> Just r
		_ -> Nothing
	_ -> Nothing

data Door = White | Black deriving Show

ctd :: Char -> Door
ctd '.' = White
ctd '*' = Black

type Doors = [Door]

instance SourceList Door where
	data ListPos Door = NoPos
	listToken [] = Nothing
	listToken (c : s) = Just (c, s)
	listInitialPos = NoPos
	listUpdatePos _ _ = NoPos

[papillon|

source: Doors

start :: [Door]
	= White a:roomA		{ White : a }
	/ Black b:roomB		{ Black : b }
	/			{ [] }

roomA :: [Door]
	= White s:start		{ White : s }
	/ Black d:dragon	{ Black : d }

dragon :: [Door]
	= White b:roomB		{ White : b }
	/ Black a:roomA		{ Black : a }

roomB :: [Door]
	= White d:dragon	{ White : d }
	/ Black s:start		{ Black : s }

|]
