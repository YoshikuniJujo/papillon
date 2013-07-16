import Text.Papillon
import System.Environment
import System.IO
import System.Directory
import System.FilePath
import Data.List

main :: IO ()
main = do
	args@(fn : _) <- getArgs
	(mn, psrc, src) <- papillonStr =<< readFile fn
	cnst <- papillonConstant
	case args of
		[_] -> do
			putStr $ psrc ++ "\n" ++ src ++ "\n" ++ cnst
			hPutStrLn stderr $ show mn
		[_, dist] -> do
			let	dir = joinPath $ dist : myInit mn
				mName = intercalate "." $ myInit mn ++ ["Papillon"]
			createDirectoryIfMissing True dir
			writeFile (dir </> takeBaseName fn <.> "hs") $
				psrc ++ "\nimport " ++ mName ++ "\n" ++
				src -- ++ "\n" ++ cnst
			writeFile (dir </> "Papillon" <.> "hs") $
				"module " ++ mName ++
				" (ParseError(..)) where\n" ++
				"import Control.Monad.Trans.Error (Error(..))\n" ++
				cnst
		_ -> error "bad arguments"

myInit :: [a] -> [a]
myInit [] = []
myInit [_] = []
myInit (x : xs) = x : myInit xs
