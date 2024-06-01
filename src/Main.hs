{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main (main) where

import           Control.Applicative
--import           Debug.Trace                (traceShowM)
import qualified Options.Applicative as O
import           System.Exit         (exitFailure)
import           System.IO
    ( IOMode (ReadMode)
    , hGetContents'
    , hPrint
    , openFile
    , stderr
    , stdin
    )
import           Text.RawString.QQ
import           Tomlq.Doc
import           Tomlq.Types

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

-- traceShowM' :: (Applicative m, Show s) => s -> m ()
-- traceShowM' = traceShowM
traceShowM' :: Applicative m => a -> m ()
traceShowM' = const (pure ())

main :: IO ()
main = do
  args <- O.execParser parseOptions
  traceShowM' args
  content <- readFileOrStdin (filePath args)
  let result = processQueries (queryStr args) content
  case head <$> result of
    Left err    -> hPrint stderr err >> exitFailure
    Right value -> putStrLn $ showValue value
  where
    readFileOrStdin :: String -> IO String
    readFileOrStdin path = do
      handle <- if path == "-" then pure stdin else openFile path ReadMode
      hGetContents' handle
