module Main where
import Control.Monad (liftM)
import Haft.ORM.Old
import Haft.ORM.YamlSchema
import Haft.Routing
import Text.Yaml
import System (getArgs, exitFailure)

loadSchema :: String -> IO [Table]
loadSchema siteName = do
	s <- readFile $ siteName ++ "/schema.yml"
	return $ yamlToTables $ decodeYaml s

loadRoutes :: String -> IO [Route]
loadRoutes siteName = do
	s <- readFile $ siteName ++ "/routing.yml"
	return $ parseRoutes $ decodeYaml s

writeSchema :: String -> String -> [Table] -> IO ()
writeSchema siteName prefix ts = do
	writeFile (siteName ++ "/ORM.hs") $ dumpSchemaHs siteName ts
	writeFile (siteName ++ "/schema.sql") $ dumpSchema prefix ts

writeRoutes :: String -> [Route] -> IO ()
writeRoutes siteName rs = 
	writeFile (siteName ++ "/RoutingTable.hs") $ dumpRoutes siteName rs

main = do
	args <- getArgs

	(siteName, prefix) <- case length args of
		1 -> return (args !! 0, "")
		2 -> return (args !! 0, args !! 1)
		_ -> do
			putStrLn $ "Usage: ./Build siteName [tablePrefix]"
			exitFailure

	writeSchema siteName prefix =<< loadSchema siteName 
	writeRoutes siteName =<< loadRoutes siteName
