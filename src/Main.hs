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
  = QueryParsingError
  | IndexOnTable
  | KeyOnArray
  | ValueNotLeaf
  | IndexOutOfRange
  | KeyError String
  | NotEnoughValues
  | TomlError
  | PastLeafNode

instance Show Error where
  show QueryParsingError = "Error parsing query"
  show IndexOnTable      = "Trying to index a table"
  show KeyOnArray        = "Trying to look "
  show ValueNotLeaf      = "Querying a non-leaf node"
  show IndexOutOfRange   = "Index out of range"
  show (KeyError k)      = "KeyError: '" ++ k ++ "'"
  show NotEnoughValues   = "Leaf node reached"
  show TomlError         = "Error parsing TOML file"
  show PastLeafNode      = "Trying to query past leaf node"

data Args
  = Args
  { queryStr :: !String
  , filePath :: !String
  } deriving Show


---- Querty parsing
parseQuery :: Parser [Subquery]
parseQuery = (parseSubquery `sepBy` char subquerySep) <* eof
  where
    parseSubqueryKey :: Parser Subquery
    parseSubqueryKey = Key <$> some (alphaNumChar <|> char '_')

    parseSubqueryIndex :: Parser Subquery
    parseSubqueryIndex = char '[' >> Index <$> L.decimal <* char ']'

    parseSubquery :: Parser Subquery
    parseSubquery = parseSubqueryKey <|> parseSubqueryIndex

-- Check if the first subquery is of Key k
parseQuery' :: String -> Either Error Query
parseQuery' queryString = do
  query <- maybe (Left QueryParsingError) Right (parseMaybe parseQuery queryString)
  case query of
    []          -> Left QueryParsingError
    (Index _:_) -> Left QueryParsingError
    (k:ks)      -> Right (k NL.:| ks)

----
listIndex :: Int -> [a] -> Either Error a
listIndex i l | i < 0 = Left IndexOutOfRange
              | otherwise = case (l, i) of
                   ([], _)   -> Left IndexOutOfRange
                   (x:_, 0)  -> Right x
                   (_:xs, _) -> listIndex (i-1) xs

getValue :: Subquery -> Value -> Either Error Value
getValue (Index i) (Array arr) = case listIndex i arr of
  Right (Array _) -> Left ValueNotLeaf
  Right (Table _) -> Left ValueNotLeaf
  Right value     -> Right value
  Left e          -> Left e
getValue (Key k) (Table t)     = case maybe (Left (KeyError k)) Right (M.lookup k t) of
  Right (Array _) -> Left ValueNotLeaf
  Right (Table _) -> Left ValueNotLeaf
  Right value     -> Right value
  Left e          -> Left e
getValue k v                   = traceShowM' (show k ++ "->" ++ show v) >> Left PastLeafNode

queryValue :: Query -> Value -> Either Error Value
queryValue (q NL.:| []) a@(Array _) = getValue q a
queryValue (q NL.:| []) t@(Table _) = getValue q t
queryValue (_ NL.:| []) _ = Left NotEnoughValues
queryValue (q NL.:| (k:ks)) value =
  let nl = k NL.:| ks in
    case (q, value) of
      (Key _, Array _)       -> Left KeyOnArray
      (Index _, Table _)     -> Left IndexOnTable
      (Key k', Table table)  ->
        maybe (Left (KeyError k')) (queryValue nl) (M.lookup k' table)
      (Index idx, Array arr) -> listIndex idx arr >>= queryValue nl
      _                      -> Left NotEnoughValues

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

valueToStr :: Value -> String
valueToStr v = case v of
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

traceShowM' :: (Applicative m, Show s) => s -> m ()
traceShowM' = traceShowM
-- traceShowM' :: Applicative m => a -> m ()
-- traceShowM' = const (pure ())

main :: IO ()
main = do
  args <- O.execParser parseOptions
  traceShowM' args
  content <- readFileOrStdin (filePath args)
  let result = do
        query <- parseQuery' (queryStr args)
        table <- either (const (Left TomlError)) Right (TM.parse content)
        queryValue query (Table table)
  case result of
    Left err    -> do
        hPrint stderr err >> exitFailure
    Right value -> putStrLn $ valueToStr value
  where
    readFileOrStdin :: String -> IO String
    readFileOrStdin path = do
      handle <- if path == "-" then pure stdin else openFile path ReadMode
      hGetContents' handle
