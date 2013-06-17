import Text.Papillon

import Language.Haskell.TH
import Language.Haskell.TH.Quote

main :: IO ()
main = print . ppr =<<
	runQ (quoteDec papillon "some :: Char\n\t= d:[isDigit] l:[isLower]\t{ d }")
