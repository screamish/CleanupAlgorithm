{-# LANGUAGE QuasiQuotes #-}

module LibSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Text.InterpolatedString.Perl6 (q)
import           Control.Lens ((<&>))
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)

import           Lib

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "Cleanup" $ do
    it "can ToJSON an X" $ do
      encode (X {foo = "2", bar = Just "12", baz = Just "6", quux = True})
        `shouldBe` [q|{ foo: "2", bar: "12", baz: "6", quux: true }|]

    it "can parse an X" $ do
      eitherDecode' [q|{ "foo": "2", "bar": "12", "baz": "6", "quux": true }|]
        `shouldBe` Right (X {foo = "2", bar = Just "12", baz = Just "6", quux = True})
    it "can parse an X" $ do
      eitherDecode' [q|{ "foo": "5", "bar": null, "baz": null, "quux": false }|]
        `shouldBe` Right (X {foo = "5", bar = Nothing, baz = Nothing, quux = False})

    it "can parse a list of X" $ do
      let output :: [X]
          output = []
          input :: ByteString
          input = [q|[{ "foo": "2", "bar": "12", "baz": "6", "quux": true },
                      { "foo": "10", "bar": "4", "baz": "17", "quux": false },
                      { "foo": "5", "bar": null, "baz": null, "quux": false },
                      { "foo": "8", "bar": "2", "baz": "12", "quux": false },
                      { "foo": "-2", "bar": "2", "baz": "9", "quux": false },
                      { "foo": "-8", "bar": "17", "baz": "8", "quux": false },
                      { "foo": "9", "bar": "18", "baz": null, "quux": true },
                      { "foo": "6", "bar": "14", "baz": null, "quux": false },
                      { "foo": "6", "bar": null, "baz": null, "quux": true },
                      { "foo": "-999", "bar": "18", "baz": "14", "quux": true },
                      { "foo": "-999", "bar": "18", "baz": "9", "quux": true },
                      { "foo": "-6", "bar": "12", "baz": "17", "quux": true },
                      { "foo": "-9", "bar": "12", "baz": "12", "quux": true },
                      { "foo": "-999", "bar": "20", "baz": "3", "quux": true },
                      { "foo": "-6", "bar": "5", "baz": "6", "quux": true },
                      { "foo": "5", "bar": "15", "baz": "1", "quux": true },
                      { "foo": "-8", "bar": "0", "baz": "10", "quux": false },
                      { "foo": "0", "bar": "9", "baz": "17", "quux": false }
                      ]
                    |]
      eitherDecode' input `shouldBe` Right output

    it "can parse a list of X" $ do
      let output :: [X]
          output = []
          input = [q|[{ "foo": "2", "bar": "12", "baz": "6", "quux": true },
                       { "foo": "10", "bar": "4", "baz": "17", "quux": false }]
                     |]
      eitherDecode' input `shouldBe` Right output

    it "process the list of data" $ do
      let input :: ByteString
          input = [q|[{ "foo": "2", "bar": "12", "baz": "6", "quux": true },
                      { "foo": "10", "bar": "4", "baz": "17", "quux": false },
                      { "foo": "5", "bar": null, "baz": null, "quux": false },
                      { "foo": "8", "bar": "2", "baz": "12", "quux": false },
                      { "foo": "-2", "bar": "2", "baz": "9", "quux": false },
                      { "foo": "-8", "bar": "17", "baz": "8", "quux": false },
                      { "foo": "9", "bar": "18", "baz": null, "quux": true },
                      { "foo": "6", "bar": "14", "baz": null, "quux": false },
                      { "foo": "6", "bar": null, "baz": null, "quux": true },
                      { "foo": "-999", "bar": "18", "baz": "14", "quux": true },
                      { "foo": "-999", "bar": "18", "baz": "9", "quux": true },
                      { "foo": "-6", "bar": "12", "baz": "17", "quux": true },
                      { "foo": "-9", "bar": "12", "baz": "12", "quux": true },
                      { "foo": "-999", "bar": "20", "baz": "3", "quux": true },
                      { "foo": "-6", "bar": "5", "baz": "6", "quux": true },
                      { "foo": "5", "bar": "15", "baz": "1", "quux": true },
                      { "foo": "-8", "bar": "0", "baz": "10", "quux": false },
                      { "foo": "0", "bar": "9", "baz": "17", "quux": false }
                      ]
                    |]
      (eitherDecode' input <&> algorithm) `shouldBe` Right (Counter {
          _foos = [2, 10, 5, 8, 2, 8, 9, 6, 6, 0, 0, 6, 9, 0, 6, 5, 8, 0]
        , _bars = [12, 4, 4, 2, 2, 17, 18, 14, 14, 18, 18, 12, 12, 20, 5, 15, 0, 9]
        , _bazs = [6, 17, 17, 12, 9, 8, 8, 8, 8, 14, 9, 17, 12, 3, 6, 1, 10, 17]
        })
