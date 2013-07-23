import System.Environment
import Data.Char

main :: IO ()
main = do
	fp : _ <- getArgs
	cnt <- readFile fp
	print $ length $ filter (not . all isSpace) $ lines cnt
