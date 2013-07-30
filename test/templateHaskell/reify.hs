{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Reify

$(testReify >>= runIO . print >> return [])
$(reify 'True >>= runIO . print >> return [])
$(reify 'Just >>= runIO . print >> return [])

main :: IO ()
main = putStrLn "end"
