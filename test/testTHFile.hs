{-# LANGUAGE TemplateHaskell #-}

import MkParser

main :: IO ()
main = do
	case dv_pegFile $ parse $ "some\n[papillon|\nheko :: Int\n\t= h:hage p:[aAdxy hoge]h:hige { hoge }/" ++
			"p:posoZ{ boka }\n;hage::Boke=b:bo { boke };\n|]\nother" of
		Just ((_, (n, _, _) : _, _), _d) -> print n
		_ -> putStrLn "bad"
