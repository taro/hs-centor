module VideoFuncs (videoInfo, takeScreenshots) where
import Control.Concurrent (forkIO)
import Control.Monad (liftM)
import Text.Regex.Posix ((=~))
import System.Directory (renameFile)
import System.Directory.WithCwd (withCwd)
import System.IO (hClose, hGetContents)
import System.Process (runProcess, waitForProcess)
import System.Process.Run (readProcess)

{-|
	Takes the path to a video file and runs it through mplayer with the
	arguments

		mplayer -identify -frames 0 <path>

	This causes mplayer to dump all the video information (but not actually
	play the video). All the info dumped by mplayer is in the form
	ID_(field)=(value) -- this is returned as a [(String, String)].
-}
videoInfo 
	:: String	-- ^ Path to the video file 
	-> IO [(String, String)]
videoInfo vidPath = do
	let cmd = "mplayer"
	let args = ["-identify", "-frames", "0", vidPath]

	Right rawOut <- readProcess cmd args ""
	return $ map (\[_, x, y] -> (x, y)) $ rawOut =~ "ID_([A-Z_]+)=(.*)"

{-|
	Function which dumps screengrabs from a video file at specified
	intervals to a specific directory. It wraps the following mplayer
	command (for each offset):

		mplayer -ss <offset> -frames 1 -vo jpeg:outdir=<outdir> <video>
-}
takeScreenshots 
	:: String	-- ^ Path to the video file 
	-> String	-- ^ Path to the output directory 
	-> [Int]	-- ^ List of offsets (in seconds) to dump at 
	-> IO ()
takeScreenshots _ _ [] = return ()
takeScreenshots vidPath outDir (offset : rest) = do
	let cmd = "mplayer"
	let args = ["-ss", show offset, "-frames", "1", "-vo", "jpeg:outdir=" ++ outDir, vidPath]
	waitForProcess =<< runProcess cmd args Nothing Nothing Nothing Nothing Nothing
	withCwd outDir $ renameFile "00000001.jpg" $ show offset ++ ".jpg"
	takeScreenshots vidPath outDir rest
