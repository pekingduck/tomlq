{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main (main) where

import           Control.Applicative        hiding (many, some)
import           Data.Map                   as M
import           Data.Void                  (Void)
--import           Debug.Trace                (traceShowM)
import qualified Options.Applicative        as O
import           System.Exit                (exitFailure)
import           System.IO
    ( IOMode (ReadMode)
    , hGetContents'
    , hPrint
    , openFile
    , stderr
    , stdin
    )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.RawString.QQ
import           Toml                       as TM

data Subquery = Key !String | Index !Int

instance Show Subquery where
  show (Key s)   = "key: " ++ s
  show (Index i) = "[" ++ show i ++ "]"

-- The first subquery must always be Key
data Query = Query !String ![Subquery] deriving Show

type Parser = Parsec Void String

data Error
  = InvalidQuery
  | IndexOnTable
  | KeyOnArray
  | NonLeafNode
  | IndexOutOfRange !Int
  | KeyError !String
  | TomlError
  | PastLeafNode

instance Show Error where
  show InvalidQuery        = "Invalid query"
  show IndexOnTable        = "Trying to index a table"
  show KeyOnArray          = "Trying to key into an array"
  show NonLeafNode         = "Querying a non-leaf node"
  show (IndexOutOfRange i) = "Index out of range: " ++ show i
  show (KeyError k)        = "Key does not exist: '" ++ k ++ "'"
  show TomlError           = "Invalid TOML file"
  show PastLeafNode        = "Trying to query past leaf node"

-- Command line args
data Args
  = Args
  { queryStr :: !String
  , filePath :: !String
  } deriving Show

---- Query parsing
parseQuery :: String -> Either Error Query
parseQuery queryString =
  case parseMaybe parseQuery' queryString of
    Nothing    -> Left InvalidQuery
    Just query -> Right query

  where
    subquerySep :: Char
    subquerySep = '.'

    parseQuery' :: Parser Query
    parseQuery' = do
      -- E.g. For query = "a.b.c",
      -- k would be "a",
      -- and maybeQuery would be Just [Key "b", Key "b"]
      Key k <- parseSubqueryKey
      -- Need optional since it could be empty
      maybeQuery <- optional (char subquerySep >> parseSubqueries <* eof)
      pure $ maybe (Query k []) (Query k) maybeQuery

    parseSubqueries :: Parser [Subquery]
    parseSubqueries = parseSubquery `sepBy` char subquerySep

    parseSubqueryKey :: Parser Subquery
    parseSubqueryKey = Key <$> some (alphaNumChar <|> char '_')

    parseSubqueryIndex :: Parser Subquery
    parseSubqueryIndex = char '[' >> Index <$> L.decimal <* char ']'

    parseSubquery :: Parser Subquery
    parseSubquery = parseSubqueryKey <|> parseSubqueryIndex

---- Query a list
queryList :: Int -> Int -> [a] -> Either Error a
queryList ogIndex index list | index < 0 = Left (IndexOutOfRange ogIndex)
                     | otherwise = case (list, index) of
                         ([], _)   -> Left (IndexOutOfRange ogIndex)
                         (x:_, 0)  -> Right x
                         (_:xs, _) -> queryList ogIndex (index-1) xs

-- Query a table
queryTable :: String -> Table -> Either Error Value
queryTable key table = maybe (Left (KeyError key)) Right (M.lookup key table)

-- Query a value
queryValue :: [Subquery] -> Value -> Either Error Value
queryValue [] (Array _) = Left NonLeafNode
queryValue [] (Table _) = Left NonLeafNode
queryValue [] value = Right value
queryValue (q:qs) value =
  case (q, value) of
    (Key k', Table table)  -> queryTable k' table >>= queryValue qs
    (Index idx, Array arr) -> queryList idx idx arr >>= queryValue qs
    (Key _, Array _)       -> Left KeyOnArray
    (Index _, Table _)     -> Left IndexOnTable
    _otherwise             -> Left PastLeafNode

-- CLI
helpText :: String
helpText = [r|
Query: key1.key2.[index], e.g.'database.ports.[1]'.
File: TOML file path or '-' for stdin.
|]

parseOptions :: O.ParserInfo Args
parseOptions = O.info (argParse <**> O.helper)
  (O.fullDesc
   <> O.progDesc helpText
   <> O.header "TOML file query tool")
  where
    argParse :: O.Parser Args
    argParse = Args
      <$> O.strOption ( O.short 'q' <> O.metavar "QUERY")
      <*> O.strArgument ( O.metavar "FILE" )

-- Output related
showValue :: Value -> String
showValue v = case v of
  String s      -> s
  Integer i     -> show i
  Float f       -> show f
  Bool b        -> if b then "true" else "false"
  TimeOfDay tmd -> show tmd
  ZonedTime zt  -> show zt
  LocalTime lt  -> show lt
  Day d         -> show d
  Array _       -> error "Array returned, please file bug report"
  Table _       -> error "Table returned, please file bug report"

-- traceShowM' :: (Applicative m, Show s) => s -> m ()
-- traceShowM' = traceShowM
traceShowM' :: Applicative m => a -> m ()
traceShowM' = const (pure ())

main :: IO ()
main = do
  args <- O.execParser parseOptions
  traceShowM' args
  content <- readFileOrStdin (filePath args)
  let result = do
        Query k query <- parseQuery (queryStr args)
        table <- either (const (Left TomlError)) Right (TM.parse content)
        val <- maybe (Left (KeyError k)) Right (M.lookup k table)
        queryValue query val
  case result of
    Left err    -> do
        hPrint stderr err >> exitFailure
    Right value -> putStrLn $ showValue value
  where
    readFileOrStdin :: String -> IO String
    readFileOrStdin path = do
      handle <- if path == "-" then pure stdin else openFile path ReadMode
      hGetContents' handle
