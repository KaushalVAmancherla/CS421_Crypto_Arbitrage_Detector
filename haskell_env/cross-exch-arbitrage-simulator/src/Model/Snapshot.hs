{-|
Module      : Model.Snapshot
Description : Types and JSON parsing for exchange snapshot data
Copyright   : (c) Kaushala Amancherla, 2025
License     : MIT

This module defines the in-memory representation of a per-exchange snapshot
of market data and provides FromJSON instances to decode the raw NDJSON/JSON
messages produced by the dataset.

Notes:
- The snapshot contains an OHLC map keyed by trading symbol.
- Tick values are decoded from JSON strings into Doubles.
- Parsing is conservative: failures in numeric parsing will propagate as parse errors.
-}

module Model.Snapshot where

-- Aeson for JSON parsing
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Aeson.KeyMap qualified as KM

-- Core data structures
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

-- | A market snapshot for a single exchange at a single timestamp.
data Snapshot = Snapshot
  { -- | ISO-8601 timestamp string for the snapshot (e.g. "2025-04-22T12:03:00")
    datetime :: Text,
    -- | Exchange identifier (e.g. "Binance")
    exchange :: Text,
    -- | Map from trading symbol to OHLC tick data
    ohlc :: Map Text Tick
  }
  deriving (Show)

-- | OHLC tick data for a single trading symbol
data Tick = Tick
  { open :: Double,
    high :: Double,
    low :: Double,
    close :: Double
  }
  deriving (Show)

-- | FromJSON instance for Tick.
-- The source JSON encodes numeric fields as strings; we parse them using 'read'.
-- If parsing fails, Aeson's parse failure will be returned.
instance FromJSON Tick where
  parseJSON = withObject "Tick" $ \v -> do
    oStr <- v .: "open"
    hStr <- v .: "high"
    lStr <- v .: "low"
    cStr <- v .: "close"

    -- Convert numeric strings to Doubles; allow Aeson to report failures
    let o = read oStr :: Double
        h = read hStr :: Double
        l = read lStr :: Double
        c = read cStr :: Double

    pure $ Tick o h l c

-- | FromJSON instance for Snapshot.
-- We parse 'datetime' and 'exchange' explicitly and treat the remaining
-- object fields as the OHLC map keyed by symbol.
instance FromJSON Snapshot where
  parseJSON = withObject "Snapshot" $ \v -> do
    dt <- v .: "datetime"
    exch <- v .: "exchange"

    -- Remove known keys and treat the rest as symbol -> Tick mappings
    let rest = KM.delete "datetime" (KM.delete "exchange" v)
    ohlcVals <- traverse parseJSON rest

    -- Convert KeyMap to a Map keyed by Text
    let list = KM.toMapText ohlcVals
    pure $ Snapshot dt exch list