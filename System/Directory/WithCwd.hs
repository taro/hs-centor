module System.Directory.WithCwd (withCwd) where
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.IO (FilePath)

{-| 
	Helper function to perform an IO operation within the context
	of another directory without actually having to change cwd
-}
withCwd 
	:: FilePath {-| Path to directory to execute action in -}
	-> IO a {-| Action to execute in alternate cwd -}
	-> IO a
withCwd path f = do
	cwd <- getCurrentDirectory
	setCurrentDirectory path
	ret <- f
	setCurrentDirectory cwd
	return ret
