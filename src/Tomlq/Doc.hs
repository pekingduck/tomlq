{-# LANGUAGE OverloadedStrings #-}

module Tomlq.Doc
  ( queryList
  , queryTable
  , queryValue
  , showValue
  , parseTOML
  , processQueries
  ) where

import           Control.Monad (forM)
import           Data.Map      as M
import           Toml          as TM

import           Tomlq.Parser
import           Tomlq.Types

---- Query a list
---- ogIndex - the original index used for composing error message
queryList :: Int -> [a] -> Either Error a
queryList ogIndex list =
  if ogIndex < 0
  then Left (IndexOutOfRange ogIndex)
  else go ogIndex list
  where
    go _ []         = Left (IndexOutOfRange ogIndex)
    go 0 (x:_)      = Right x
    go index (_:xs) = go (index-1) xs

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
    (Index idx, Array arr) -> queryList idx arr >>= queryValue qs
    (Key _, Array _)       -> Left KeyOnArray
    (Index _, Table _)     -> Left IndexOnTable
    _otherwise             -> Left PastLeafNode

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

parseTOML :: String -> Either Error (Map String Value)
parseTOML content = do
  table <- either (const (Left TomlError)) Right (TM.parse content)
  if M.null table then Left EmptyDoc else Right table

processQueries :: String -> String -> Either Error [Value]
processQueries queryString content = do
  queries <- parseQueries queryString
  doc <- parseTOML content
  forM queries $ \(Query k query) -> do
    val <- maybe (Left (KeyError k)) Right (M.lookup k doc)
    queryValue query val
