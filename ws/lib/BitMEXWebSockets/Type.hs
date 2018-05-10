module BitMEXWebSockets.Type
    ( Command(..)
    , Topic(..)
    , Message(..)
    , Symbol(..)
    , Response(..)
    ) where

import qualified BitMEX.Model     as M
    ( APIKey
    , AccessToken
    , Affiliate
    , Announcement
    , Chat
    , ChatChannels
    , ConnectedUsers
    , Error
    , Execution
    , Funding
    , IndexComposite
    , Instrument
    , InstrumentInterval
    , Insurance
    , Leaderboard
    , Liquidation
    , Margin
    , Notification
    , Order
    , OrderBookL2
    , Position
    , Quote
    , Settlement
    , Stats
    , StatsHistory
    , StatsUSD
    , Trade
    , TradeBin
    , Transaction
    , User
    , UserCommission
    , UserPreferences
    , Wallet
    )
import           Control.Monad    (fail)
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
    , withObject
    , (.:)
    , (.:?)
    )
import           Data.Aeson.Types (Parser)
import           Data.Char        (toLower)
import           Data.Text        (Text, append, pack)
import           GHC.Generics
import           Prelude
    ( Bool (..)
    , Double
    , Eq
    , Integer
    , Maybe
    , Maybe (..)
    , Show
    , drop
    , return
    , show
    , ($)
    , (++)
    , (.)
    , (<$>)
    , (<*>)
    )

data Side
    = Buy
    | Sell
    deriving (Eq, Show, Generic)

instance ToJSON Side

instance FromJSON Side

data Action
    = Partial
    | Update
    | Insert
    | Delete
    deriving (Eq, Show, Generic)

instance ToJSON Action

instance FromJSON Action where
    parseJSON = genericParseJSON opts
      where
        opts =
            defaultOptions
            { constructorTagModifier =
                  (\(x:xs) -> (toLower x : xs))
            }

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
    , args :: ![a]
    } deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Message a)

instance FromJSON a => FromJSON (Message a)

data Table a = Table
    { _table       :: !Text
    , _action      :: !Action
    , _data        :: ![a]
    , _keys        :: !(Maybe [Text])
    , _foreignKeys :: !(Maybe Value)
    , _types       :: !(Maybe Value)
    , _filter      :: !(Maybe Value)
    , _attributes  :: !(Maybe Value)
    } deriving (Eq, Show, Generic)

instance (ToJSON a) => ToJSON (Table a)

instance (FromJSON a) => FromJSON (Table a)

data BitMEXStatus = BitMEXStatus
    { success   :: !Bool
    , subscribe :: !(Maybe Text)
    , request   :: !Value
    } deriving (Eq, Show, Generic)

instance ToJSON BitMEXStatus

instance FromJSON BitMEXStatus

data BitMEXInfo = BitMEXInfo
    { info      :: !Text
    , version   :: !Text
    , timestamp :: !Text
    , docs      :: !Text
    , limit     :: !Value
    } deriving (Eq, Show, Generic)

instance ToJSON BitMEXInfo

instance FromJSON BitMEXInfo

data BitMEXError = BitMEXError
    { error :: !Text
    } deriving (Eq, Show, Generic)

instance ToJSON BitMEXError

instance FromJSON BitMEXError

data Response
    = Ann (Table M.Announcement)
    | OB (Table M.OrderBookL2)
    | Status BitMEXStatus
    | Info BitMEXInfo
    | Error BitMEXError
    deriving (Eq, Show, Generic)

instance FromJSON Response where
    parseJSON =
        withObject "Response" $ \o -> do
            kind <- o .:? "table"
            success <- o .:? "success"
            info <- o .:? "info"
            error <- o .:? "error"
            case (kind :: Maybe Text) of
                Just "orderBookL2" ->
                    OB <$> genericParseJSON opts (Object o)
                Just "announcement" ->
                    Ann <$> genericParseJSON opts (Object o)
                Nothing ->
                    case (success :: Maybe Bool) of
                        Just _ ->
                            Status <$> parseJSON (Object o)
                        Nothing ->
                            case (info :: Maybe Text) of
                                Just _ ->
                                    Info <$>
                                    parseJSON (Object o)
                                Nothing ->
                                    case (error :: Maybe Text) of
                                        Just _ ->
                                            Error <$>
                                            parseJSON
                                                (Object o)
                                        Nothing ->
                                            fail
                                                "something went wrong"
      where
        opts =
            defaultOptions
            { fieldLabelModifier = drop 1
            , sumEncoding = UntaggedValue
            }
