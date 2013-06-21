{-# LANGUAGE TypeFamilies #-}

class Source sl where
	type Pos sl :: *
	showPos :: Pos sl -> String

class SourceList c where
	data ListPos c :: *
	listShowPos :: ListPos c -> String

instance (SourceList c) => Source [c] where
	type Pos [c] = ListPos c
	showPos p = listShowPos p
