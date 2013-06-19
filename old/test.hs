{-# LANGUAGE TemplateHaskell #-}

import Text.Papillon.Parser
import Language.Haskell.TH

main = do
	simple <- readFile "simple.peg"
	case dvDefinition $ parse simple of
		Just ((v, t, [([(tv, Right p)],b)]), d) -> do
			print (v, t)
			print =<< runQ p
			print =<< runQ b
			print . ppr =<< runQ (p `appE` litE (CharL 'c'))
		Nothing -> error "bad"
