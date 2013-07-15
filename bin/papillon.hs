import Text.Papillon
import System.Environment

main :: IO ()
main = do
	fn : _ <- getArgs
	putStr =<< papillonStr =<< readFile fn
