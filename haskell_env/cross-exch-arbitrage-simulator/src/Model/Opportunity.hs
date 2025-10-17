module Model.Opportunity where

import           Data.Text       (Text)

{--
This data type represents a valid arbitrage opportunity, encapsulating the necessary metadata into this object
--}

-- | A single cross-exchange arbitrage opportunity.
data Opportunity = Opportunity
  { arSymbol :: !Text
  , arBuyEx  :: !Text
  , arBuyPx  :: !Double
  , arSellEx :: !Text
  , arSellPx :: !Double
  } deriving (Eq, Show)