module Msum1 (
	msum1
) where

import Control.Monad

msum1 :: MonadPlus m => [m a] -> m a
msum1 = foldl1 mplus
