{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Control.Lens
import Control.Lens.TH
import Control.Error.Util ((?:))
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import GHC.Generics
import Text.InterpolatedString.Perl6 (q)

data X = X {
    foo  :: String
  , bar  :: Maybe String
  , baz  :: Maybe String
  , quux :: Bool
  } deriving (Show, Eq, Generic)

instance FromJSON X where

instance ToJSON X where
    toEncoding = genericToEncoding defaultOptions

data Counter = Counter {
    _foos :: [Integer]
  , _bars :: [Integer]
  , _bazs :: [Integer]
  } deriving (Show, Eq)

makeLenses ''Counter

algorithm :: [X] -> Counter
algorithm = rev . foldl g empty . scanning
  where g c (foo, bar, baz) = c & foos %~ (foo :)
                                & bars %~ (bar :)
                                & bazs %~ (baz :)
        rev c = c & foos %~ reverse
                  & bars %~ reverse
                  & bazs %~ reverse
        empty = Counter [] [] []

scanning :: [X] -> [(Integer, Integer, Integer)]
scanning = drop 1 . scanl f (0,0,0)
  where
    f (foo', bar', baz') X{..} = (foo'', bar'', baz'')
      where
        -- The javascript source this was based on didn't gracefully
        -- handle parse/read failures, so I'm not either
        foo'' = case read foo of
                  -999 -> 0
                  i    -> abs i
        bar'' = fmap read bar ?: bar'
        baz'' = fmap read baz ?: baz'

input :: [X]
input = let (Right i) =
              eitherDecode'
                [q| [ { "foo": "2", "bar": "12", "baz": "6", "quux": true },
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
        in i
