{-# LANGUAGE PackageImports #-}

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Monad.Trans.Error (Error(..))

import Msum1

type PackratM = StateT Int (Either String)

some, some' :: PackratM Int
some = return 8
some' = fail "hoge"
some'' = throwError (strMsg "hoge")

other :: PackratM Int
other = return 99

sando :: PackratM Int
sando = some'' `mplus` other
