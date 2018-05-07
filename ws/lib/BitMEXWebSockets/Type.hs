module BitMEXWebSockets.Type
    ( Command(..)
    , Topic(..)
    , Message(..)
    , Symbol(..)
    , Response(..)
    ) where

import           Data.Aeson
    ( FromJSON
    , SumEncoding (UntaggedValue)
    , ToJSON
    , Value (..)
    , constructorTagModifier
    , defaultOptions
    , fieldLabelModifier
    , genericParseJSON
    , genericToJSON
    , parseJSON
    , sumEncoding
    , toJSON
    )
import           Data.Char    (toLower)
import           Data.Text    (Text, append, pack)
import           GHC.Generics
import           Prelude
    ( Bool
    , Double
    , Eq
    , Integer
    , Show
    , drop
    , show
    , (.)
    )

data Side
    = Buy
    | Sell
    deriving (Eq, Show, Generic)

instance ToJSON Side

instance FromJSON Side

data Symbol
    = XBTUSD
    | XBTM18
    | XBTU18
    | XBT7D_U110
    | ADAM18
    | BCHM18
    | ETHM18
    | LTCM18
    | XRPM18
    deriving (Eq, Show, Generic)

instance ToJSON Symbol

instance FromJSON Symbol

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
                  (\(x:xs) -> (toLower x : xs))
            }

instance FromJSON Command

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
                  (\(x:xs) -> (toLower x : xs))
            }

instance (FromJSON a) => FromJSON (Topic a)

data Message a = Message
    { op   :: !Command
    , args :: [a]
    } deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Message a)

instance FromJSON a => FromJSON (Message a)

data Orders = Orders
    { symbol :: !Symbol
    , id     :: !Integer
    , side   :: !Side
    , size   :: !Integer
    , price  :: !Double
    } deriving (Eq, Show, Generic)

instance FromJSON Orders

instance ToJSON Orders

data Response
    = Table { _table       :: !Text
            , _keys        :: ![Text]
            , _types       :: !Value
            , _foreignKeys :: !Value
            , _attributes  :: !Value
            , _action      :: !Text
            , _data        :: [Orders] }
    | Status { _success   :: !Bool
             , _subscribe :: !Text
             , _request   :: !Value }
    deriving (Eq, Show, Generic)

instance FromJSON Response where
    parseJSON = genericParseJSON opts
      where
        opts =
            defaultOptions
            { fieldLabelModifier = drop 1
            , sumEncoding = UntaggedValue
            }
