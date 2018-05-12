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
    , object
    , sumEncoding
    , toJSON
    , (.=)
    )
import           Data.Char                      (toLower)
import           Data.Text
    ( Text
    , append
    , pack
    )
import           Data.Vector
    ( Vector
    , (!)
    )
import           GHC.Generics
import           Prelude
    ( Eq
    , Show
    , String
    , show
    , (++)
    , (.)
    )
import qualified Prelude                        as P
    ( String
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
                  \(x:xs) -> (toLower x : xs)
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
    toJSON t@(OrderBookL2 x) =
        String (append "orderBookL2:" ((pack . show) x))
    toJSON t@(OrderBook10 x) =
        String (append "orderBook10:" ((pack . show) x))
    toJSON t@(QuoteBin1m x) =
        String (append "quoteBin1m:" ((pack . show) x))
    toJSON t@(QuoteBin5m x) =
        String (append "quoteBin5m:" ((pack . show) x))
    toJSON t@(QuoteBin1h x) =
        String (append "quoteBin1h:" ((pack . show) x))
    toJSON t@(QuoteBin1d x) =
        String (append "quoteBin1d:" ((pack . show) x))
    toJSON t@(Trade x) =
        String (append "trade:" ((pack . show) x))
    toJSON t@(TradeBin1m x) =
        String (append "tradeBin1m:" ((pack . show) x))
    toJSON t@(TradeBin5m x) =
        String (append "tradeBin5m:" ((pack . show) x))
    toJSON t@(TradeBin1h x) =
        String (append "tradeBin1h:" ((pack . show) x))
    toJSON t@(TradeBin1d x) =
        String (append "tradeBin1d:" ((pack . show) x))
    toJSON x = genericToJSON opts x
      where
        opts =
            defaultOptions
            { constructorTagModifier =
                  \(x:xs) -> (toLower x : xs)
            , sumEncoding = UntaggedValue
            }

data Message a = Message
    { op   :: !Command
    , args :: !(Vector a)
    } deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Message a)
