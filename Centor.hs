module Main where
import Control.Exception (throw, handle, Exception (NoMethodError, ErrorCall))
import Control.Monad (liftM)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Database.HDBC (commit, disconnect, IConnection (..))
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Haft.Routing
import Network.CGI
import Network.URI (uriPath)
import System.Directory (setCurrentDirectory)

import Centor.Init

main :: IO ()
main = do
	-- XXX
	setCurrentDirectory "/usr/home/hark/dev/hs-centor/"
	db <- liftIO $ connectPostgreSQL "user=hark"
	at <- getActionTable db "Centor/Templates/"
	ut <- getUrlTree

	runCGI $ handleErrors $ handler db ut at
		`catchCGI` (\e -> (liftIO $ disconnect db) >> throwCGI e)

resolveFormatMime :: String -> String
resolveFormatMime format = fromMaybe "text/html" $ lookup format [
	("html", "text/html"),
	("xml", "text/xml"),
	("rss", "application/rss+xml"),
	("json", "text/plain")
	]

handler :: IConnection a => a -> UrlTree -> ActionTable -> CGI CGIResult
handler db ut at = do
	setHeader "Content-Type" "text/plain"

	-- XXX
	let baseurl = "/centor/"

	fullurl <- liftM uriPath requestURI

	if not $ isPrefixOf baseurl fullurl
		then throwCGI $ NoMethodError $ unlines [
			"uri isn't in baseurl.",
			"uri: " ++ fullurl,
			"baseurl: " ++ baseurl
			]
		else return ()

	let req = (drop (length baseurl) fullurl)
	let (action, format) = route ut at req

	res <- liftIO action

	setHeader "Content-Type" $ resolveFormatMime format

	liftIO $ commit db

	output res
