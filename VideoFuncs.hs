module VideoFuncs (videoInfo, takeScreenshots) where
import Control.Concurrent (forkIO)
import Control.Monad (liftM)
import Text.Regex.Posix ((=~))
import System.Directory (renameFile)
import System.IO (hClose, hGetContents)
import System.Process (runProcess, waitForProcess)
import System.Process.Run (readProcess)

videoInfo :: String -> IO [(String, String)]
videoInfo vidPath = do
	let cmd = "mplayer"
	let args = ["-identify", "-frames", "0", vidPath]

	Right rawOut <- readProcess cmd args ""
	return $ map (\[_, x, y] -> (x, y)) $ rawOut =~ "ID_([A-Z_]+)=(.*)"

takeScreenshots :: String -> String -> [Int] -> IO ()
takeScreenshots _ _ [] = return ()
takeScreenshots vidPath outDir (offset : rest) = do
	let cmd = "mplayer"
	let args = ["-ss", show offset, "-frames", "1", "-vo", "jpeg:outdir=" ++ outDir, vidPath]
	waitForProcess =<< runProcess cmd args Nothing Nothing Nothing Nothing Nothing
	renameFile "00000001.jpg" $ show offset ++ ".jpg"
	takeScreenshots vidPath outDir rest
