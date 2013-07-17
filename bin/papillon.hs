import Text.PapillonCore
import System.Environment
import System.Directory
import System.FilePath
import Data.List
import Language.Haskell.TH

import Class

papillonStr :: String -> IO (String, String, String)
papillonStr src = do
	let 	(prgm, mn, ppp, pp, decsQ, atp, app) = papillonFile src
		mName = intercalate "." $ myInit mn ++ ["Papillon"]
		importConst = "\nimport " ++ mName ++ "\n"
		dir = joinPath $ myInit mn
	decs <- runQ decsQ
	return (dir, mName,
		showPragma prgm ++
		(if null mn then "" else "module " ++ intercalate "." mn) ++
		ppp ++ importConst ++
		(if app then "\nimport Control.Applicative\n" else "") ++
		pp ++ "\n" ++ show (ppr decs) ++ "\n" ++ atp ++ "\n")

papillonConstant :: String -> IO String
papillonConstant mName = do
	src <- runQ $ do
		pe <- parseErrorT False
		iepe <- instanceErrorParseError False
		pepst <- pePositionST
		pepsd <- pePositionSD
		cls <- classSourceQ False
		return $ [pe, iepe, pepst, pepsd] ++ cls
	return $
		"{-# LANGUAGE RankNTypes, TypeFamilies #-}\n" ++
		"module " ++ mName ++ " (\n\t" ++
		intercalate ",\n\t" exportList ++ ") where\n" ++
		"import Control.Monad.Trans.Error (Error(..))\n" ++
		show (ppr src) ++ "\n"

main :: IO ()
main = do
	args <- getArgs
	case args of
		[fn, dist] -> do
			(d, mName, src) <- papillonStr =<< readFile fn
			let dir = dist </> d
			createDirectoryIfMissing True dir
			writeFile (dir </> takeBaseName fn <.> "hs") src
			writeFile (dir </> "Papillon" <.> "hs")
				=<< papillonConstant mName
		_ -> error "bad arguments"

exportList :: [String]
exportList = [
	"ParseError(..)",
	"Pos(..)",
	"pePositionS",
	"Source(..)",
	"SourceList(..)",
	"ListPos(..)"
 ]

myInit :: [a] -> [a]
myInit [] = []
myInit [_] = []
myInit (x : xs) = x : myInit xs
