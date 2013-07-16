module Text.Papillon.Papillon (ParseError(..)) where
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
