module BitMEXClient.WebSockets.Types.Request
    ( Command(..)
    , Topic(..)
    , Message(..)
    ) where

import           BitMEXClient.CustomPrelude
import           BitMEXClient.WebSockets.Types.General
import qualified Data.Text                             as T
    ( append
    , pack
    )
import           Data.Vector
    ( Vector
    )

data Command
    = Subscribe
    | Unsubscribe
    | Ping
    | CancelAllAfter
    | AuthKey
    deriving (Eq, Show, Generic)

instance ToJSON Command where
    toJSON = genericToJSON opts
      where
        opts =
            defaultOptions
            { constructorTagModifier =
                  \xs -> case xs of
                      []        -> xs
                      (x : xs') -> (toLower x : xs')
            }

data Topic a
    = Announcement
    | Chat
    | Connected
    | Funding
    | Instrument
    | Insurance
    | Liquidation
    | OrderBookL2 Symbol
    | OrderBook10 Symbol
    | PublicNotifications
    | Quote Symbol
    | QuoteBin1m Symbol
    | QuoteBin5m Symbol
    | QuoteBin1h Symbol
    | QuoteBin1d Symbol
    | Settlement
    | Trade Symbol
    | TradeBin1m Symbol
    | TradeBin5m Symbol
    | TradeBin1h Symbol
    | TradeBin1d Symbol
    | Affiliate
    | Execution
    | Order
    | Margin
    | Position
    | PrivateNotifications
    | Transact
    | Wallet
    deriving (Eq, Show, Generic)

instance (ToJSON a) => ToJSON (Topic a) where
    toJSON (OrderBookL2 v) =
        String (T.append "orderBookL2:" ((T.pack . show) v))
    toJSON (OrderBook10 v) =
        String (T.append "orderBook10:" ((T.pack . show) v))
    toJSON (QuoteBin1m v) =
        String (T.append "quoteBin1m:" ((T.pack . show) v))
    toJSON (QuoteBin5m v) =
        String (T.append "quoteBin5m:" ((T.pack . show) v))
    toJSON (QuoteBin1h v) =
        String (T.append "quoteBin1h:" ((T.pack . show) v))
    toJSON (QuoteBin1d v) =
        String (T.append "quoteBin1d:" ((T.pack . show) v))
    toJSON (Trade v) =
        String (T.append "trade:" ((T.pack . show) v))
    toJSON (TradeBin1m v) =
        String (T.append "tradeBin1m:" ((T.pack . show) v))
    toJSON (TradeBin5m v) =
        String (T.append "tradeBin5m:" ((T.pack . show) v))
    toJSON (TradeBin1h v) =
        String (T.append "tradeBin1h:" ((T.pack . show) v))
    toJSON (TradeBin1d v) =
        String (T.append "tradeBin1d:" ((T.pack . show) v))
    toJSON v = genericToJSON opts v
      where
        opts =
            defaultOptions
            { constructorTagModifier =
                  \xs -> case xs of
                      []        -> xs
                      (x : xs') -> (toLower x : xs')
            , sumEncoding = UntaggedValue
            }

data Message a = Message
    { op   :: !Command
    , args :: !(Vector a)
    } deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Message a)
