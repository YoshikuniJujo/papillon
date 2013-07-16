{-# LANGUAGE RankNTypes, TypeFamilies #-}
module Text.Papillon.Papillon (
	ParseError(..),
	Pos(..),
	pePositionS,
	Source(..),
	SourceList(..),
	ListPos(..)) where
import Control.Monad.Trans.Error (Error(..))
data ParseError pos drv
    = ParseError {peCode :: String,
                  peMessage :: String,
                  peComment :: String,
                  peDerivs :: drv,
                  peReading :: ([String]),
                  pePosition :: pos}
instance Error (ParseError pos drv)
    where strMsg msg = ParseError "" msg "" undefined undefined undefined
pePositionS :: forall drv . ParseError (Pos String) drv ->
                            (Int, Int)
pePositionS (ParseError {pePosition = ListPos (CharPos p)}) = p
class Source sl
    where type Token sl
          data Pos sl
          getToken :: sl -> Maybe ((Token sl, sl))
          initialPos :: Pos sl
          updatePos :: Token sl -> Pos sl -> Pos sl
class SourceList c
    where data ListPos c
          listToken :: [c] -> Maybe ((c, [c]))
          listInitialPos :: ListPos c
          listUpdatePos :: c -> ListPos c -> ListPos c
instance SourceList c => Source ([c])
    where type Token ([c]) = c
          newtype Pos ([c]) = ListPos (ListPos c)
          getToken = listToken
          initialPos = ListPos listInitialPos
          updatePos c (ListPos p) = ListPos (listUpdatePos c p)
instance SourceList Char
    where newtype ListPos Char = CharPos ((Int, Int)) deriving (Show)
          listToken (c : s) = Just (c, s)
          listToken _ = Nothing
          listInitialPos = CharPos (1, 1)
          listUpdatePos '\n' (CharPos (y, _)) = CharPos (y + 1, 0)
          listUpdatePos _ (CharPos (y, x)) = CharPos (y, x + 1)
