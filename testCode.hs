import Text.Papillon

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import System.Environment

main :: IO ()
main = do
	args <- getArgs
	case args of
		["-q"] -> print . ppr =<< runQ (quoteDec papillon test)
		_ -> putStrLn =<< papillonStr test

test :: String
test = "some :: Char\n\t= d:[isDigit] l:[isLower]\t{ d };"
