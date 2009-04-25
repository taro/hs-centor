module Haft.ORM.YamlSchema where
import Data.Maybe (fromMaybe, maybe)
import Haft.ORM.Base
import Text.Yaml (YamlValue (..), YamlObject (..))

yamlToType :: YamlValue -> ColType
yamlToType (YString "Text") = ColText
yamlToType (YString "Datetime") = ColDatetime
yamlToType (YString "Integer") = ColInteger
yamlToType (YHash h) =
	case lookup "type" h of
		Just (YString "Reference") -> ColReference $ mur uwYString $ lookup "references" h
		Just (YString "String") -> ColString $ mur uwYInteger $ lookup "length" h
	where
		mur x = maybe undefined x
		uwYString (YString s) = s
		uwYInteger (YInteger i) = i

yamlToColumn :: (String, YamlValue) -> (String, ColType)
yamlToColumn (colName, colType) = (colName, yamlToType colType)

yamlToTable :: YamlObject -> Table
yamlToTable (YamlObject (tableName, cols)) = 
	(tableName, map yamlToColumn cols)

yamlToTables :: YamlObject -> [Table]
yamlToTables (YamlObject ("schema", tables)) =
	map (yamlToTable . uwYObject . snd) tables
	where
		uwYObject (YObject o) = o
