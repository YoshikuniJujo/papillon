{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import Text.Papillon

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
	/ Black eye:openEye	{ Black : eye }

roomA :: [Door]
	= White eye:closeEye	{ White : eye }
	/ Black eye:openEye	{ Black : eye }

openEye :: [Door]
	= White b:roomB		{ White : b }
	/ Black c:roomC		{ Black : c }

closeEye :: [Door]
	= Black d:roomD		{ Black : d }

roomB :: [Door]
	= White d:roomD		{ White : d }

roomC :: [Door]
	= White			{ [White] }

roomD :: [Door]
	= Black			{ [Black] }

|]
