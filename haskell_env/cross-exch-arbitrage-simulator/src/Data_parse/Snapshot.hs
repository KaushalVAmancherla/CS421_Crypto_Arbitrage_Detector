{-# LANGUAGE OverloadedStrings #-}

module Data_parse.Snapshot where

import           Data.Aeson              (FromJSON (..), withObject, (.:))
import qualified Data.Aeson.KeyMap       as KM
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Text               (Text)

{--
Data types to represnet a Snapshot
--}

data Snapshot = Snapshot
  {
    datetime :: Text,
    exchange :: Text,
    ochl :: Map Text Tick
  } deriving Show

data Tick = Tick
  { open     :: Double
  , high     :: Double
  , low      :: Double
  , close    :: Double
  } deriving Show

{--
Methods to praes the raw JSON and populate instances of the defined data types above
--}

instance FromJSON Tick where
  parseJSON = withObject "Tick" $ \v -> do
    oStr <- v .: "open"
    hStr <- v .: "high"
    lStr <- v .: "low"
    cStr <- v .: "close"

    -- parse the strings into Doubles
    let o = read oStr :: Double
        h = read hStr
        l = read lStr
        c = read cStr

    pure $ Tick o h l c

instance FromJSON Snapshot where
  parseJSON = withObject "Snapshot" $ \v -> do
    dt   <- v .: "datetime"
    exchange <- v .: "exchange"

    let rest = KM.delete "datetime" (KM.delete "exchange" v)
    ohlc <- traverse parseJSON rest

    let list = KM.toMapText ohlc
    pure $ Snapshot dt exchange list