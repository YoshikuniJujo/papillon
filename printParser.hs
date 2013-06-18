import Text.Papillon
import Language.Haskell.TH
import Language.Haskell.TH.Quote

main :: IO ()
main = do
	cnt <- readFile "test.peg"
	putStr =<< papillonStr cnt
