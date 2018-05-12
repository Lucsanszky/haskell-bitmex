module BitMEXWebSockets.Types.Response
    ( Response(..)
    ) where

import qualified BitMEX.Model                   as M
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
import           BitMEXWebSockets.Types.General
import           Control.Monad                  (fail)
import           Data.Aeson
    ( FromJSON
    , SumEncoding (UntaggedValue)
    , ToJSON
    , Value (..)
    , constructorTagModifier
    , defaultOptions
    , fieldLabelModifier
    , genericParseJSON
    , parseJSON
    , sumEncoding
    , withObject
    , (.:)
    , (.:?)
    )
import           Data.Char                      (toLower)
import           Data.Text                      (Text)
import           Data.Vector                    (Vector)
import           GHC.Generics
import           Prelude
    ( Bool
    , Double
    , Eq
    , Integer
    , Maybe (..)
    , Show
    , drop
    , ($)
    , (<$>)
    , (<*>)
    )

data Side
    = Buy
    | Sell
    deriving (Eq, Show, Generic)

instance FromJSON Side

data Action
    = Partial
    | Update
    | Insert
    | Delete
    deriving (Eq, Show, Generic)

instance FromJSON Action where
    parseJSON = genericParseJSON opts
      where
        opts =
            defaultOptions
            { constructorTagModifier =
                  \(x:xs) -> (toLower x : xs)
            }

data TABLE a = TABLE
    { _table       :: !Text
    , _action      :: !Action
    , _data        :: !(Vector a)
    , _keys        :: !(Maybe (Vector Text))
    , _foreignKeys :: !(Maybe Value)
    , _types       :: !(Maybe Value)
    , _filter      :: !(Maybe Value)
    , _attributes  :: !(Maybe Value)
    } deriving (Eq, Show, Generic)

instance (FromJSON a) => FromJSON (TABLE a)

data STATUS = STATUS
    { success   :: !Bool
    , subscribe :: !(Maybe Text)
    , request   :: !Value
    } deriving (Eq, Show, Generic)

instance FromJSON STATUS

data INFO = INFO
    { info      :: !Text
    , version   :: !Text
    , timestamp :: !Text
    , docs      :: !Text
    , limit     :: !Value
    } deriving (Eq, Show, Generic)

instance FromJSON INFO

newtype ERROR = ERROR
    { error :: Text
    } deriving (Eq, Show, FromJSON, Generic)

data RespOrderBook10 = RespOrderBook10
    { symbol    :: !Symbol
    , timestamp :: !Text
    , asks      :: !(Vector (Vector Double))
    , bids      :: !(Vector (Vector Double))
    } deriving (Eq, Show, Generic)

instance FromJSON RespOrderBook10

data Response
    = AK (TABLE M.APIKey)
    | AT (TABLE M.AccessToken)
    | Aff (TABLE M.Affiliate)
    | Ann (TABLE M.Announcement)
    | C (TABLE M.Chat)
    | CC (TABLE M.ChatChannels)
    | CU (TABLE M.ConnectedUsers)
    | Err (TABLE M.Error)
    | Exe (TABLE M.Execution)
    | F (TABLE M.Funding)
    | IC (TABLE M.IndexComposite)
    | I (TABLE M.Instrument)
    | II (TABLE M.InstrumentInterval)
    | Insu (TABLE M.Insurance)
    | LB (TABLE M.Leaderboard)
    | L (TABLE M.Liquidation)
    | M (TABLE M.Margin)
    | N (TABLE M.Notification)
    | O (TABLE M.Order)
    | OB (TABLE M.OrderBookL2)
    | OB10 (TABLE RespOrderBook10)
    | P (TABLE M.Position)
    | Q (TABLE M.Quote)
    | Setl (TABLE M.Settlement)
    | S (TABLE M.Stats)
    | SH (TABLE M.StatsHistory)
    | SU (TABLE M.StatsUSD)
    | T (TABLE M.Trade)
    | TB (TABLE M.TradeBin)
    | TX (TABLE M.Transaction)
    | U (TABLE M.User)
    | UC (TABLE M.UserCommission)
    | UP (TABLE M.UserPreferences)
    | W (TABLE M.Wallet)
    | Status STATUS
    | Info INFO
    | Error ERROR
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
                    OB10 <$>
                    genericParseJSON opts (Object o)
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
