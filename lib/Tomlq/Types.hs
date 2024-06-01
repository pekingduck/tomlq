{-# LANGUAGE QuasiQuotes #-}

module Tomlq.Types
  ( Subquery(..)
  , Query(..)
  , Error(..)
  , Args(..)
  ) where

data Subquery = Key !String | Index !Int

instance Show Subquery where
  show (Key s)   = "(Key:" ++ s ++ ")"
  show (Index i) = "[" ++ show i ++ "]"

-- Query cannot be empty and the first subquery must always be Key
-- (i.e. !String)
data Query = Query !String ![Subquery] deriving Show

data Error
  = InvalidQuery
  | IndexOnTable
  | KeyOnArray
  | NonLeafNode
  | IndexOutOfRange !Int
  | KeyError !String
  | TomlError
  | PastLeafNode
  | EmptyDoc

instance Show Error where
  show InvalidQuery        = "Invalid query"
  show IndexOnTable        = "Trying to index a table"
  show KeyOnArray          = "Trying to key into an array"
  show NonLeafNode         = "Querying a non-leaf node"
  show (IndexOutOfRange i) = "Index out of range: " ++ show i
  show (KeyError k)        = "Key does not exist: '" ++ k ++ "'"
  show TomlError           = "Invalid TOML file"
  show PastLeafNode        = "Trying to query past leaf node"
  show EmptyDoc            = "Input is empty"

-- Command line args
data Args
  = Args
  { queryStr :: !String
  , filePath :: !String
  } deriving Show
