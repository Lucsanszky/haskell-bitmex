module BitMEXWebSockets.Types.Request
    ( Command(..)
    , Topic(..)
    , Message(..)
    ) where

import           BitMEXWebSockets.Types.General
import           Data.Aeson
    ( SumEncoding (UntaggedValue)
    , ToJSON
    , Value (String)
    , constructorTagModifier
    , defaultOptions
    , genericToJSON
    , sumEncoding
    , toJSON
    )
import           Data.Char                      (toLower)
import           Data.Text
    ( append
    , pack
    )
import           Data.Vector                    (Vector)
import           GHC.Generics
import           Prelude
    ( Eq
    , Show
    , show
    , (.)
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
        String (append "orderBookL2:" ((pack . show) v))
    toJSON (OrderBook10 v) =
        String (append "orderBook10:" ((pack . show) v))
    toJSON (QuoteBin1m v) =
        String (append "quoteBin1m:" ((pack . show) v))
    toJSON (QuoteBin5m v) =
        String (append "quoteBin5m:" ((pack . show) v))
    toJSON (QuoteBin1h v) =
        String (append "quoteBin1h:" ((pack . show) v))
    toJSON (QuoteBin1d v) =
        String (append "quoteBin1d:" ((pack . show) v))
    toJSON (Trade v) =
        String (append "trade:" ((pack . show) v))
    toJSON (TradeBin1m v) =
        String (append "tradeBin1m:" ((pack . show) v))
    toJSON (TradeBin5m v) =
        String (append "tradeBin5m:" ((pack . show) v))
    toJSON (TradeBin1h v) =
        String (append "tradeBin1h:" ((pack . show) v))
    toJSON (TradeBin1d v) =
        String (append "tradeBin1d:" ((pack . show) v))
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
