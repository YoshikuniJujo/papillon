import Text.Papillon
import System.Environment
import System.IO
import System.Directory
import System.FilePath

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
			let	dir = joinPath $ dist : init mn
			createDirectoryIfMissing True dir
			writeFile (dir </> takeBaseName fn <.> "hs") $
				psrc ++ "\n" ++ src ++ "\n" ++ cnst
		_ -> error "bad arguments"
