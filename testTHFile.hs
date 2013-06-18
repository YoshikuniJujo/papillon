{-# LANGUAGE TemplateHaskell #-}

import Text.Papillon
import MkParser

main :: IO ()
main = do
	case dv_peg $ parse $ "heko :: Int\n\t= h:hage p:[aAdxy hoge]h:hige { hoge }/" ++
			"p:posoZ{ boka }\n;hage=bo { boke };' 3" of
		Just (r@((n, _, _) : _), _d) -> print n
		_ -> putStrLn "bad"

