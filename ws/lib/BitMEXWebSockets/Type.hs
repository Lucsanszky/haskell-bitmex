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
                  \(x:xs) -> (toLower x : xs)
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
                  \(x:xs) -> (toLower x : xs)
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
                  \(x:xs) -> (toLower x : xs)
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

newtype BitMEXError = BitMEXError
    { error :: Text
    } deriving (Eq, Show, ToJSON, FromJSON, Generic)

data ResponseOrderBook10 = ResponseOrderBook10
    { symbol    :: !Symbol
    , timestamp :: !Text
    , asks      :: !(Vector (Vector Double))
    , bids      :: !(Vector (Vector Double))
    } deriving (Eq, Show, Generic)

instance ToJSON ResponseOrderBook10
instance FromJSON ResponseOrderBook10

data Response
    = AK (Table M.APIKey)
    | AT (Table M.AccessToken)
    | Aff (Table M.Affiliate)
    | Ann (Table M.Announcement)
    | C (Table M.Chat)
    | CC (Table M.ChatChannels)
    | CU (Table M.ConnectedUsers)
    | Err (Table M.Error)
    | Exe (Table M.Execution)
    | F (Table M.Funding)
    | IC (Table M.IndexComposite)
    | I (Table M.Instrument)
    | II (Table M.InstrumentInterval)
    | Insu (Table M.Insurance)
    | LB (Table M.Leaderboard)
    | L (Table M.Liquidation)
    | M (Table M.Margin)
    | N (Table M.Notification)
    | O (Table M.Order)
    | OB (Table M.OrderBookL2)
    | OB10 (Table ResponseOrderBook10)
    | P (Table M.Position)
    | Q (Table M.Quote)
    | Setl (Table M.Settlement)
    | S (Table M.Stats)
    | SH (Table M.StatsHistory)
    | SU (Table M.StatsUSD)
    | T (Table M.Trade)
    | TB (Table M.TradeBin)
    | TX (Table M.Transaction)
    | U (Table M.User)
    | UC (Table M.UserCommission)
    | UP (Table M.UserPreferences)
    | W (Table M.Wallet)
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
                Just "announcement" ->
                    Ann <$> genericParseJSON opts (Object o)
                Just "affiliate" ->
                    Aff <$> genericParseJSON opts (Object o)
                Just "chat" ->
                    C <$> genericParseJSON opts (Object o)
                Just "connected" ->
                    CU <$> genericParseJSON opts (Object o)
                Just "execution" ->
                    Exe <$> genericParseJSON opts (Object o)
                Just "funding" ->
                    F <$> genericParseJSON opts (Object o)
                Just "instrument" ->
                    I <$> genericParseJSON opts (Object o)
                Just "insurance" ->
                    Insu <$>
                    genericParseJSON opts (Object o)
                Just "liquidation" ->
                    L <$> genericParseJSON opts (Object o)
                Just "margin" ->
                    M <$> genericParseJSON opts (Object o)
                Just "order" ->
                    O <$> genericParseJSON opts (Object o)
                Just "orderBookL2" ->
                    OB <$> genericParseJSON opts (Object o)
                Just "orderBook10" ->
                    OB10 <$> genericParseJSON opts (Object o)
                Just "position" ->
                    P <$> genericParseJSON opts (Object o)
                Just "privateNotifications" ->
                    N <$> genericParseJSON opts (Object o)
                Just "publicNotifications" ->
                    N <$> genericParseJSON opts (Object o)
                Just "quote" ->
                    Q <$> genericParseJSON opts (Object o)
                Just "settlement" ->
                    Setl <$>
                    genericParseJSON opts (Object o)
                Just "trade" ->
                    T <$> genericParseJSON opts (Object o)
                Just "transact" ->
                    TX <$> genericParseJSON opts (Object o)
                Just "wallet" ->
                    W <$> genericParseJSON opts (Object o)
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
