{-# LANGUAGE QuasiQuotes #-}

import Text.Papillon

type Nil = ()
nil :: Nil
nil = ()

cons = (:)
empty = []
true = True

isOpenBr, isP, isA, isI, isL, isO, isN, isBar :: Char -> Bool
[isOpenBr, isP, isA, isI, isL, isO, isN, isBar] = map (==) "[pailon|"

[papillon|

prePap :: String
	= s:string2 ss:string		{ s }
;
string2 :: String
	= p:![isOpenBr] c:[const true] s:string2	{ cons c s }
	/						{ empty }
;
string_ :: String
	= p:!pap c:[const true] s:string_		{ cons c s }
	/						{ empty }
;
string :: String
	= c:[const true] s:string	{ cons c s }
	/				{ empty }
;
pap :: Nil
	= ob:[isOpenBr] p:[isP] a:[isA] pp:[isP] i:[isI] l:[isL] ll:[isL]
		o:[isO] n:[isN] bar:[isBar]	{ nil }
;
|]
