{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main (main) where

import           Control.Applicative        hiding (many, some)
import qualified Data.List.NonEmpty         as NL
import           Data.Map                   as M
import           Data.Void                  (Void)
import           Debug.Trace                (traceShowM)
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

subquerySep :: Char
subquerySep = '.'

type Query = NL.NonEmpty Subquery

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
  | QueryMismatch

instance Show Error where
  show InvalidQuery        = "Invalid query"
  show IndexOnTable        = "Trying to index a table"
  show KeyOnArray          = "Trying to key into an array"
  show NonLeafNode         = "Querying a non-leaf node"
  show (IndexOutOfRange i) = "Index out of range: " ++ show i
  show (KeyError k)        = "Key does not exist: '" ++ k ++ "'"
  show TomlError           = "Invalid TOML file"
  show PastLeafNode        = "Trying to query past leaf node"
  show QueryMismatch       = "Query mismatch"

data Args
  = Args
  { queryStr :: !String
  , filePath :: !String
  } deriving Show


---- Querty parsing
parseQuery' :: Parser [Subquery]
parseQuery' = (parseSubquery `sepBy` char subquerySep) <* eof
  where
    parseSubqueryKey :: Parser Subquery
    parseSubqueryKey = Key <$> some (alphaNumChar <|> char '_')

    parseSubqueryIndex :: Parser Subquery
    parseSubqueryIndex = char '[' >> Index <$> L.decimal <* char ']'

    parseSubquery :: Parser Subquery
    parseSubquery = parseSubqueryKey <|> parseSubqueryIndex

-- Check if the first subquery is of Key k
parseQuery :: String -> Either Error Query
parseQuery queryString = do
  query <- maybe (Left InvalidQuery) Right (parseMaybe parseQuery' queryString)
  case query of
    []          -> Left InvalidQuery
    (Index _:_) -> Left InvalidQuery
    (k:ks)      -> Right (k NL.:| ks)

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
queryValue :: Query -> Value -> Either Error Value
queryValue (q NL.:| []) a@(Array _) = getLeaf q a
queryValue (q NL.:| []) t@(Table _) = getLeaf q t
queryValue (_ NL.:| []) _ = Left PastLeafNode
queryValue (q NL.:| (k:ks)) value =
  let query  = k NL.:| ks in
    case (q, value) of
      (Key k', Table table)  -> queryTable k' table >>= queryValue query
      (Index idx, Array arr) -> queryList idx idx arr >>= queryValue query
      (Key _, Array _)       -> Left KeyOnArray
      (Index _, Table _)     -> Left IndexOnTable
      _otherwise             -> Left PastLeafNode

-- Reach the last subquery and try to retrieve a simple (i.e. non-table,
-- non-list) value
getLeaf :: Subquery -> Value -> Either Error Value
getLeaf subquery value =
  let result =
        case (subquery, value) of
          (Index index, Array arr) -> queryList index index arr
          (Key key, Table table)   -> queryTable key table
          (Index _, Table _)       -> Left IndexOnTable
          (Key _, Array _)         -> Left KeyOnArray
          _result                  -> Left QueryMismatch
   in case result of
        Left e          -> Left e
        Right (Array _) -> Left NonLeafNode
        Right (Table _) -> Left NonLeafNode
        Right value'    -> Right value'

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
        query <- parseQuery (queryStr args)
        table <- either (const (Left TomlError)) Right (TM.parse content)
        queryValue query (Table table)
  case result of
    Left err    -> do
        hPrint stderr err >> exitFailure
    Right value -> putStrLn $ showValue value
  where
    readFileOrStdin :: String -> IO String
    readFileOrStdin path = do
      handle <- if path == "-" then pure stdin else openFile path ReadMode
      hGetContents' handle
