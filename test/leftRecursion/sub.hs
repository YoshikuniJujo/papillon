import Data.Char

test :: String
test = "5-4-2"

data RSub = RSub Int RSub | RNum Int deriving Show
data LSub = LSub LSub Int | LNum Int deriving Show

-- Exp = Num '-' Exp / Num

right :: String -> RSub
right [c]
	| isDigit c = RNum $ read [c]
	| otherwise = error "not parsed"
right (c : '-' : cs)
	| isDigit c = read [c] `RSub` right cs
	| otherwise = error "not parsed"

-- Exp = Exp '-' Num / Num
--
-- Exp = Num Exp'
-- Exp' = '-' Num Exp' / {- empty -}

left :: String -> LSub
left (c : cs)
	| isDigit c = left' (LNum $ read [c]) cs
	| otherwise = error "not parsed"

left' :: LSub -> String -> LSub
left' r [] = r
left' r ('-' : c : cs)
	| isDigit c = left' (r `LSub` read [c]) cs
	| otherwise = error "not parsed"
