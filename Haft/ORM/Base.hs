{-# LANGUAGE TemplateHaskell #-}
module Haft.ORM.Base where

import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Regex.Posix

{-|
	Type definitions for all of the SQL types. To add a new one, you
	have to add it here, then create a corresponding genTypeHs and genTypeSql
	function (I guess I should enforce that statically).
-}
data ColType = 
	{-| A reference to another table. Creates a foreign key. -}
	ColReference String |
	{-| A vanilla INTEGER -}
	ColInteger |
	{-| A VARCHAR(x) -}
	ColString Integer |
	{-| A TEXT field -}
	ColText |
	{-| A timestamp. Kind of on the fence about handling these -- since
	    HDBC has some issues with TIMESTAMP WITHOUT TIME ZONE, ugh -}
	ColDatetime
	deriving (Show)

{-|
	Type definition for a table. Eventually this should be wrapped up
	in a Yaml parser or something, but for now you have to define your
	tables in Haskell somewhere.

	A table is a (TableName, [(ColumnName, ColumnType)]).

	An "Id" column (for the primary key) is generated automatically.
-}
type Table = (String, [(String, ColType)])

{-| Helper function which capitalizes the first letter of a string -}
capitalize :: String -> String
capitalize "" = ""
capitalize s = (toUpper . head) s : tail s

{-| Helper function which uncapitalizes the first letter of a string -}
uncapitalize :: String -> String
uncapitalize "" = ""
uncapitalize s = (toLower . head) s : tail s
