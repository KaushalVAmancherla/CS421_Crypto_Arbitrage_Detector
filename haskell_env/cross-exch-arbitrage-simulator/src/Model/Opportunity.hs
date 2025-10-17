{-|
Module      : Model.Opportunity
Description : Representation of a validated cross-exchange arbitrage opportunity
Copyright   : (c) Kaushala Amancherla, 2025
License     : MIT

This module defines the lightweight 'Opportunity' type used to record
validated arbitrage opportunities discovered by the simulator.
-}

module Model.Opportunity where

import Data.Text (Text)

-- | A single cross-exchange arbitrage opportunity.
--
-- Fields:
-- * 'arSymbol' - trading symbol (e.g. "BTC_USD")
-- * 'arBuyEx'  - exchange to buy from
-- * 'arBuyPx'  - buy price
-- * 'arSellEx' - exchange to sell on
-- * 'arSellPx' - sell price
data Opportunity = Opportunity
  { arSymbol :: !Text
  , arBuyEx  :: !Text
  , arBuyPx  :: !Double
  , arSellEx :: !Text
  , arSellPx :: !Double
  }
  deriving (Eq, Show)