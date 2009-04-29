module Centor.Init
	( getActionTable
	, getUrlTree
	) 
	where

import Centor.RoutingTable
import Database.HDBC
import qualified Data.Map as Map
import Haft.Routing
import Text.StringTemplate (directoryGroup, STGroup (..))

getActionTable :: IConnection a => a -> String -> IO ActionTable
getActionTable db tplPath = do
	tpls <- directoryGroup tplPath :: IO (STGroup String)
	return $ Map.fromList $ map (\(k, f) -> (k, f db tpls)) rawActionTable

getUrlTree :: IO UrlTree
getUrlTree = return urlTree
