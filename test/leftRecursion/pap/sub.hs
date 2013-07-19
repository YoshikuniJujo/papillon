{-# LANGUAGE PackageImports, TypeFamilies, RankNTypes #-}
module Main where

import Papillon
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error

import Data.Char

main :: IO ()
main = case sub $ parse "8-3-2" of
	Right (r, _) -> print r
	Left _ -> error "bad"

data Derivs
    = Derivs {sub :: (Either (ParseError (Pos String) Derivs)
                             ((Int, Derivs))),
              num :: (Either (ParseError (Pos String) Derivs) ((Int, Derivs))),
              derivsChars :: (Either (ParseError (Pos String) Derivs)
                                     ((Token String, Derivs))),
              derivsPosition :: (Pos String)}
parse :: String -> Derivs
parse = parse0_0 initialPos
          where parse0_0 pos s = d
                             where d = Derivs sub6_1 num7_2 chars8_3 pos
                                   sub6_1 = runStateT sub4_4 d
                                   num7_2 = runStateT num5_5 d
                                   chars8_3 = runStateT (case getToken s of
                                                             Just (c,
                                                                   s') -> do put (parse0_0 (updatePos c pos) s')
                                                                             return c
                                                             _ -> gets derivsPosition >>= (throwError . mkParseError "" "end of input" "" undefined [])) d
                sub4_4 = foldl1 mplus [do n <- StateT num
                                          d12_6 <- get
                                          xx11_7 <- StateT derivsChars
                                          case xx11_7 of
                                              '-' -> return ()
                                              _ -> gets derivsPosition >>= (throwError . mkParseError "'-'" "not match pattern: " "" d12_6 ["derivsChars"])
                                          let '-' = xx11_7
                                          return ()
                                          s <- StateT sub
                                          return (n + negate s),
                                       do n <- StateT num
                                          return n]
                num5_5 = foldl1 mplus [do d18_8 <- get
                                          xx17_9 <- StateT derivsChars
                                          let c = xx17_9
                                          unless (isDigit c) (gets derivsPosition >>= (throwError . mkParseError "isDigit c" "not match: " "" d18_8 ["derivsChars"]))
                                          return (read [c])]

