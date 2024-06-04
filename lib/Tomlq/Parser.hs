module Tomlq.Parser ( parseQueries )where

import           Control.Applicative        hiding (many, some)
import           Data.Void                  (Void)

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Tomlq.Types

type Parser = Parsec Void String

---- Query parsing
subquerySep :: Parser Char
subquerySep = char '.'

querySep :: Parser Char
querySep = char ','

parseQueries :: String -> Either Error [Query]
parseQueries queryString =
  case parseMaybe (parseQuery `sepBy` querySep <* eof) queryString of
    Just (x:xs) -> Right (x:xs)
    _otherwise  -> Left InvalidQuery  -- include Just []

parseQuery :: Parser Query
parseQuery = do
  -- E.g. For query = "a.b.c",
  -- k would be "a",
  -- and maybeQuery would be Just [Key "b", Key "b"]
  Key k <- parseSubqueryKey
  -- Need optional since it could be empty
  maybeQuery <- optional (subquerySep >> parseSubqueries)
  pure $ maybe (Query k []) (Query k) maybeQuery

parseSubqueries :: Parser [Subquery]
parseSubqueries = parseSubquery `sepBy` subquerySep

parseSubqueryKey :: Parser Subquery
parseSubqueryKey = Key <$> some (alphaNumChar <|> char '_')

parseSubqueryIndex :: Parser Subquery
parseSubqueryIndex = char '[' >> Index <$> L.decimal <* char ']'

parseSubquery :: Parser Subquery
parseSubquery = parseSubqueryKey <|> parseSubqueryIndex
