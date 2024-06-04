{-# LANGUAGE BlockArguments #-}
module Main (main) where

import           Control.Monad (forM, forM_)
import           Data.Map      as M
import           System.Exit   (exitFailure)
import           System.IO     (readFile)
import           Test.Hspec
import           Tomlq.Doc     (parseTOML, processQuery, queryValue, showValue)
import           Tomlq.Parser  (parseQueries)
import           Tomlq.Types   (Error (..), Query (..))

data ValidTest = ValidTest !String !String !String

data InvalidTest = InvalidTest !String !Error

main = do
  content <- readFile "test.toml"
  let res = parseTOML content
  case res of
    Left err -> exitFailure
    Right doc ->
      hspec do
        let validQueries
              = [ ValidTest "Simple key" "title" "TOML Example"
                , ValidTest "Nested table" "owner.name" "Tom Preston-Werner"
                , ValidTest "Nested table - date" "owner.dob" "1979-05-27 07:32:00 -0800"
                , ValidTest "Nested table - float" "database.temp_targets.cpu" "79.5"
                , ValidTest "Nested table - array" "database.ports.[1]" "8001"
                ]
        describe "Valid queries" do
          forM_ validQueries $ \(ValidTest des queryS expected) -> do
            it des do
              let result = (head <$> parseQueries queryS) >>= (`processQuery` doc)
              (showValue <$> result) `shouldBe` Right expected

        let invalidQueries
              = [ InvalidTest "[0].owner" InvalidQuery
                , InvalidTest "owner.dob.xyz" PastLeafNode
                , InvalidTest "owner" NonLeafNode
                , InvalidTest "servers.alpha.[0]" IndexOnTable
                , InvalidTest "database.ports.0" KeyOnArray
                , InvalidTest "xxx" (KeyError "xxx")
                , InvalidTest "database.ports.[9999]" (IndexOutOfRange 9999)
                ]
        describe "Invalid queries" do
          forM_ invalidQueries $ \(InvalidTest queryS expectedError) -> do
            it (show expectedError) do
              let result = (head <$> parseQueries queryS) >>= (`processQuery` doc)
              result `shouldBe` Left expectedError

        describe "Empty tests" do
          it "errors out on empty doc" do
            parseTOML "" `shouldBe` Left EmptyDoc

          it "errors out on empty query" do
            parseQueries "" `shouldBe` Left InvalidQuery
