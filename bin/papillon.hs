import Text.Papillon
import System.Environment

main :: IO ()
main = do
	fn : _ <- getArgs
	(psrc, src) <- papillonStr =<< readFile fn
	cnst <- papillonConstant
	putStr $ psrc ++ "\n" ++ src ++ "\n" ++ cnst
