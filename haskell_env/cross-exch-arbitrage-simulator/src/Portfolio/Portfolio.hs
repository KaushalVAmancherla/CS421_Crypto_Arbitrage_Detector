module Portfolio.Portfolio where

import           Control.Concurrent.STM
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)

{--
This data type represents a valid arbitrage opportunity, encapsulating the necessary metadata into this object
--}

data ValidArbitrage = ValidArbitrage
  { sym      :: Text -- symbol
  , exBuy    :: Text -- buyside exchange
  , pBuy     :: Double -- buyside price
  , exSell   :: Text-- sellside exchange
  , pSell    :: Double -- sellside price
  } deriving (Eq, Show)