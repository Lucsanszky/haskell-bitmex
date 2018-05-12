module BitMEXWebSockets.Types.Response
    ( Response(..)
    , RespAffiliate(..)
    , RespAnnouncement(..)
    , RespChat(..)
    , RespConnectedUsers(..)
    , RespExecution(..)
    , RespFunding(..)
    , RespInsurance(..)
    , RespLiquidation(..)
    , RespNotification(..)
    , RespOrderBookL2(..)
    , RespOrderBook10(..)
    , RespQuote(..)
    , RespSettlement(..)
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

import           BitMEX.Core                    (DateTime)
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
    , Int
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

data RespAffiliate = RespAffiliate
    { account         :: !Integer -- ^ /Required/ "account"
    , currency        :: !Currency -- ^ /Required/ "currency"
    , prevPayout      :: !(Maybe Integer) -- ^ "prevPayout"
    , prevTurnover    :: !(Maybe Integer) -- ^ "prevTurnover"
    , prevComm        :: !(Maybe Integer) -- ^ "prevComm"
    , prevTimestamp   :: !(Maybe DateTime) -- ^ "prevTimestamp"
    , execTurnover    :: !(Maybe Integer) -- ^ "execTurnover"
    , execComm        :: !(Maybe Integer) -- ^ "execComm"
    , totalReferrals  :: !(Maybe Integer) -- ^ "totalReferrals"
    , totalTurnover   :: !(Maybe Integer) -- ^ "totalTurnover"
    , totalComm       :: !(Maybe Integer) -- ^ "totalComm"
    , payoutPcnt      :: !(Maybe Double) -- ^ "payoutPcnt"
    , pendingPayout   :: !(Maybe Integer) -- ^ "pendingPayout"
    , timestamp       :: !(Maybe DateTime) -- ^ "timestamp"
    , referrerAccount :: !(Maybe Double) -- ^ "referrerAccount"
    } deriving (Show, Eq, Generic)

instance FromJSON RespAffiliate

data RespAnnouncement = RespAnnouncement
    { id      :: !Integer -- ^ /Required/ "id"
    , link    :: !(Maybe Text) -- ^ "link"
    , title   :: !(Maybe Text) -- ^ "title"
    , content :: !(Maybe Text) -- ^ "content"
    , date    :: !(Maybe DateTime) -- ^ "date"
    } deriving (Show, Eq, Generic)

instance FromJSON RespAnnouncement

data RespChat = RespChat
    { id        :: !(Maybe Integer) -- ^ "id"
    , date      :: !DateTime -- ^ /Required/ "date"
    , user      :: !Text -- ^ /Required/ "user"
    , message   :: !Text -- ^ /Required/ "message"
    , html      :: !Text -- ^ /Required/ "html"
    , fromBot   :: !(Maybe Bool) -- ^ "fromBot"
    , channelId :: !(Maybe Double) -- ^ "channelID"
    } deriving (Show, Eq, Generic)

instance FromJSON RespChat

data RespConnectedUsers = RespConnectedUsers
    { connectedUsersUsers :: !(Maybe Int) -- ^ "users"
    , connectedUsersBots  :: !(Maybe Int) -- ^ "bots"
    } deriving (Show, Eq, Generic)

instance FromJSON RespConnectedUsers

data RespExecution = RespExecution
    { execId                :: !Text -- ^ /Required/ "execID"
    , orderId               :: !(Maybe Text) -- ^ "orderID"
    , clOrdId               :: !(Maybe Text) -- ^ "clOrdID"
    , clOrdLinkId           :: !(Maybe Text) -- ^ "clOrdLinkID"
    , account               :: !(Maybe Integer) -- ^ "account"
    , symbol                :: !(Maybe Symbol) -- ^ "symbol"
    , side                  :: !(Maybe Side) -- ^ "side"
    , lastQty               :: !(Maybe Integer) -- ^ "lastQty"
    , lastPx                :: !(Maybe Double) -- ^ "lastPx"
    , underlyingLastPx      :: !(Maybe Double) -- ^ "underlyingLastPx"
    , lastMkt               :: !(Maybe Text) -- ^ "lastMkt"
    , lastLiquidityInd      :: !(Maybe Text) -- ^ "lastLiquidityInd"
    , simpleOrderQty        :: !(Maybe Double) -- ^ "simpleOrderQty"
    , orderQty              :: !(Maybe Integer) -- ^ "orderQty"
    , price                 :: !(Maybe Double) -- ^ "price"
    , displayQty            :: !(Maybe Integer) -- ^ "displayQty"
    , stopPx                :: !(Maybe Double) -- ^ "stopPx"
    , pegOffsetValue        :: !(Maybe Double) -- ^ "pegOffsetValue"
    , pegPriceType          :: !(Maybe Text) -- ^ "pegPriceType"
    , currency              :: !(Maybe Currency) -- ^ "currency"
    , settlCurrency         :: !(Maybe Currency) -- ^ "settlCurrency"
    , execType              :: !(Maybe Text) -- ^ "execType"
    , ordType               :: !(Maybe Text) -- ^ "ordType"
    , timeInForce           :: !(Maybe Text) -- ^ "timeInForce"
    , execInst              :: !(Maybe Text) -- ^ "execInst"
    , contingencyType       :: !(Maybe Text) -- ^ "contingencyType"
    , exDestination         :: !(Maybe Text) -- ^ "exDestination"
    , ordStatus             :: !(Maybe Text) -- ^ "ordStatus"
    , triggered             :: !(Maybe Text) -- ^ "triggered"
    , workingIndicator      :: !(Maybe Bool) -- ^ "workingIndicator"
    , ordRejReason          :: !(Maybe Text) -- ^ "ordRejReason"
    , simpleLeavesQty       :: !(Maybe Double) -- ^ "simpleLeavesQty"
    , leavesQty             :: !(Maybe Integer) -- ^ "leavesQty"
    , simpleCumQty          :: !(Maybe Double) -- ^ "simpleCumQty"
    , cumQty                :: !(Maybe Integer) -- ^ "cumQty"
    , avgPx                 :: !(Maybe Double) -- ^ "avgPx"
    , commission            :: !(Maybe Double) -- ^ "commission"
    , tradePublishIndicator :: !(Maybe Text) -- ^ "tradePublishIndicator"
    , multiLegReportingType :: !(Maybe Text) -- ^ "multiLegReportingType"
    , text                  :: !(Maybe Text) -- ^ "text"
    , trdMatchId            :: !(Maybe Text) -- ^ "trdMatchID"
    , execCost              :: !(Maybe Integer) -- ^ "execCost"
    , execComm              :: !(Maybe Integer) -- ^ "execComm"
    , homeNotional          :: !(Maybe Double) -- ^ "homeNotional"
    , foreignNotional       :: !(Maybe Double) -- ^ "foreignNotional"
    , transactTime          :: !(Maybe DateTime) -- ^ "transactTime"
    , timestamp             :: !(Maybe DateTime) -- ^ "timestamp"
    } deriving (Show, Eq, Generic)

instance FromJSON RespExecution

data RespFunding = RespFunding
    { timestamp        :: !DateTime -- ^ /Required/ "timestamp"
    , symbol           :: !Text -- ^ /Required/ "symbol"
    , fundingInterval  :: !(Maybe DateTime) -- ^ "fundingInterval"
    , fundingRate      :: !(Maybe Double) -- ^ "fundingRate"
    , fundingRateDaily :: !(Maybe Double) -- ^ "fundingRateDaily"
    } deriving (Show, Eq, Generic)

instance FromJSON RespFunding

-- TODO: Instrument
data RespInsurance = RespInsurance
    { currency      :: !Currency -- ^ /Required/ "currency"
    , timestamp     :: !DateTime -- ^ /Required/ "timestamp"
    , walletBalance :: !(Maybe Integer) -- ^ "walletBalance"
    } deriving (Show, Eq, Generic)

instance FromJSON RespInsurance

data RespLiquidation = RespLiquidation
    { orderId   :: !Text -- ^ /Required/ "orderID"
    , symbol    :: !(Maybe Symbol) -- ^ "symbol"
    , side      :: !(Maybe Side) -- ^ "side"
    , price     :: !(Maybe Double) -- ^ "price"
    , leavesQty :: !(Maybe Integer) -- ^ "leavesQty"
    } deriving (Show, Eq, Generic)

instance FromJSON RespLiquidation

-- TODO: Margin
data RespNotification = RespNotification
    { _id                :: !(Maybe Int) -- ^ "id"
    , _date              :: !(DateTime) -- ^ /Required/ "date"
    , _title             :: !(Text) -- ^ /Required/ "title"
    , _body              :: !(Text) -- ^ /Required/ "body"
    , _ttl               :: !Int -- ^ /Required/ "ttl"
    , _type              :: !(Maybe Text) -- ^ "type"
    , _closable          :: !(Maybe Bool) -- ^ "closable"
    , _persist           :: !(Maybe Bool) -- ^ "persist"
    , _waitForVisibility :: !(Maybe Bool) -- ^ "waitForVisibility"
    , _sound             :: !(Maybe Text) -- ^ "sound"
    } deriving (Show, Eq, Generic)

instance FromJSON RespNotification where
    parseJSON = genericParseJSON opts
      where
        opts = defaultOptions {fieldLabelModifier = drop 1}

-- TODO: Order
data RespOrderBookL2 = RespOrderBookL2
    { symbol :: !Symbol -- ^ /Required/ "symbol"
    , id     :: !Integer -- ^ /Required/ "id"
    , side   :: !Side -- ^ /Required/ "side"
    , size   :: !(Maybe Integer) -- ^ "size"
    , price  :: !(Maybe Double) -- ^ "price"
    } deriving (Show, Eq, Generic)

instance FromJSON RespOrderBookL2

data RespOrderBook10 = RespOrderBook10
    { symbol    :: !Symbol
    , timestamp :: !Text
    , asks      :: !(Vector (Vector Double))
    , bids      :: !(Vector (Vector Double))
    } deriving (Eq, Show, Generic)

instance FromJSON RespOrderBook10

-- TODO: Position
data RespQuote = RespQuote
    { timestamp :: !DateTime -- ^ /Required/ "timestamp"
    , symbol    :: !Symbol -- ^ /Required/ "symbol"
    , bidSize   :: !(Maybe Integer) -- ^ "bidSize"
    , bidPrice  :: !(Maybe Double) -- ^ "bidPrice"
    , askPrice  :: !(Maybe Double) -- ^ "askPrice"
    , askSize   :: !(Maybe Integer) -- ^ "askSize"
    } deriving (Show, Eq, Generic)

instance FromJSON RespQuote

-- TODO: Add the new fields to Settlement
data RespSettlement = RespSettlement
    { timestamp      :: !DateTime -- ^ /Required/ "timestamp"
    , symbol         :: !Symbol -- ^ /Required/ "symbol"
    , settlementType :: !(Maybe Text) -- ^ "settlementType"
    , settledPrice   :: !(Maybe Double) -- ^ "settledPrice"
    , bankrupt       :: !(Maybe Integer) -- ^ "bankrupt"
    , taxBase        :: !(Maybe Integer) -- ^ "taxBase"
    , taxRate        :: !(Maybe Double) -- ^ "taxRate"
    } deriving (Show, Eq, Generic)

instance FromJSON RespSettlement

-- TODO: Trade
-- TODO: Transaction
-- TODO: Wallet
data Response
    = Aff (TABLE RespAffiliate)
    | Ann (TABLE RespAnnouncement)
    | C (TABLE RespChat)
    | CU (TABLE RespConnectedUsers)
    | Exe (TABLE RespExecution)
    | F (TABLE RespFunding)
    | I (TABLE M.Instrument)
    | Insu (TABLE RespInsurance)
    | L (TABLE RespLiquidation)
    | M (TABLE M.Margin)
    | N (TABLE RespNotification)
    | O (TABLE M.Order)
    | OB (TABLE RespOrderBookL2)
    | OB10 (TABLE RespOrderBook10)
    | P (TABLE M.Position)
    | Q (TABLE RespQuote)
    | Setl (TABLE RespSettlement)
    | T (TABLE M.Trade)
    | TB (TABLE M.TradeBin)
    | TX (TABLE M.Transaction)
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
                Just "affiliate" ->
                    Aff <$> genericParseJSON opts (Object o)
                Just "announcement" ->
                    Ann <$> genericParseJSON opts (Object o)
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
