module Text.Yaml where
import Control.Exception (throw, Exception(ErrorCall))
import Control.Monad (liftM, liftM2)
import Text.ParserCombinators.Parsec
import Text.Regex.Posix

data YamlValue 
	= YInteger Integer
	| YFloat Float
	| YString String
	| YList [YamlValue]
	| YHash [(String, YamlValue)]
	| YObject YamlObject
	deriving (Show)

data YamlObject = YamlObject (String, [(String, YamlValue)])
	deriving (Show)

decodeYaml :: String -> YamlObject
decodeYaml s = 
	let p = yvObject 0 in
	case parse p "" s of
		Left err -> throw $ ErrorCall $ show err
		Right x -> x

psymbol p = do
	skipMany $ char ' '
	v <- string p
	skipMany $ char ' '
	return v

pneg = option "" (string "-")

(<++>) = liftM2 (++)

yvInteger :: Parser YamlValue
yvInteger = do
	i <- pneg <++> many1 digit
	return $ YInteger $ read i

yvFloat :: Parser YamlValue
yvFloat = do
	s <- pneg <++> many1 digit <++> string "." <++> many1 digit
	return $ YFloat $ read s

yvString :: Parser YamlValue
yvString = try quoted <|> try unquoted 
	where
		quoted = do
			let dq = (char '"')
			s <- between dq dq $ many $ noneOf ['"', '\\']
			return $ YString s
		unquoted = liftM YString $ many1 letter

yvList :: Parser YamlValue
yvList = 
	liftM YList $ btw $ sepBy yvValue sep
	where
		btw = between (psymbol "[") (psymbol "]")
		sep = psymbol ","

yvIdentifier :: Parser String
yvIdentifier = do
	let ugh = '_' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
	x <- letter
	xs <- many $ satisfy (`elem` ugh)
	return $ x : xs

yvHashItem :: Parser (String, YamlValue)
yvHashItem = do
	i <- yvIdentifier
	psymbol ":"
	v <- yvValue
	spaces
	return (i, v)

yvHash :: Parser YamlValue
yvHash = do
	liftM YHash $ btw $ sepBy yvHashItem sep
	where
		btw = between (psymbol "{") (psymbol "}")
		sep = psymbol ","

yvObject :: Int -> Parser YamlObject
yvObject d = do
	i <- yvIdentifier
	psymbol ":"

	v <- many1 $ try $ do
		many1 $ psymbol "\n"
		count (d + 1) tab

		let pobj = do
			o <- yvObject (d + 1)
			let YamlObject (oid, vs) = o
			return $ (oid, YObject o)

		let pitem = do
			oid <- yvIdentifier
			psymbol ":"
			o <- yvValue
			return (oid, o)

		try pobj <|> pitem

	return $ YamlObject (i, v)

yvValue :: Parser YamlValue
yvValue = try yvFloat
	<|>	try yvInteger 
	<|> try yvString 
	<|> try yvList
	<|> try yvHash
