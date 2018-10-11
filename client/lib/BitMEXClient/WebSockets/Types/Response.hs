module BitMEXClient.WebSockets.Types.Response
    ( Response(..)
    , RespAffiliate(..)
    , RespAnnouncement(..)
    , RespChat(..)
    , RespConnectedUsers(..)
    , RespExecution(..)
    , RespFunding(..)
    , RespInstrument(..)
    , RespInsurance(..)
    , RespLiquidation(..)
    , RespMargin(..)
    , RespNotification(..)
    , RespOrder(..)
    , RespOrderBookL2(..)
    , RespOrderBook10(..)
    , RespPosition(..)
    , RespQuote(..)
    , RespSettlement(..)
    , RespTrade(..)
    , RespTransaction(..)
    , RespWallet(..)
    , TABLE(..)
    , STATUS(..)
    , INFO(..)
    , ERROR(..)
    , Action(..)
    ) where

import           BitMEX.Core
    ( DateTime
    )
import           BitMEXClient.CustomPrelude
import           BitMEXClient.WebSockets.Types.General
import           Data.Text
    ( Text
    )
import           Data.Vector
    ( Vector
    )

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
                  \xs ->
                      case xs of
                          []      -> xs
                          (x:xs') -> (toLower x : xs')
            }

data NotificationType
    = NERROR
    | NINFO
    | NSUCCESS
    deriving (Eq, Show, Generic)

instance FromJSON NotificationType where
    parseJSON = genericParseJSON opts
      where
        opts =
            defaultOptions
            { constructorTagModifier =
                  ((drop 1) . (map toLower))
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

data ERROR = ERROR
    { error   :: !Text
    , status  :: !(Maybe Int)
    , meta    :: !(Maybe Value)
    , request :: !(Maybe Value)
    } deriving (Eq, Show, Generic)

instance FromJSON ERROR

data RespAffiliate = RespAffiliate
    { account         :: !Double -- ^ /Required/ "account"
    , currency        :: !Currency -- ^ /Required/ "currency"
    , prevPayout      :: !(Maybe Double) -- ^ "prevPayout"
    , prevTurnover    :: !(Maybe Double) -- ^ "prevTurnover"
    , prevComm        :: !(Maybe Double) -- ^ "prevComm"
    , prevTimestamp   :: !(Maybe DateTime) -- ^ "prevTimestamp"
    , execTurnover    :: !(Maybe Double) -- ^ "execTurnover"
    , execComm        :: !(Maybe Double) -- ^ "execComm"
    , totalReferrals  :: !(Maybe Double) -- ^ "totalReferrals"
    , totalTurnover   :: !(Maybe Double) -- ^ "totalTurnover"
    , totalComm       :: !(Maybe Double) -- ^ "totalComm"
    , payoutPcnt      :: !(Maybe Double) -- ^ "payoutPcnt"
    , pendingPayout   :: !(Maybe Double) -- ^ "pendingPayout"
    , timestamp       :: !(Maybe DateTime) -- ^ "timestamp"
    , referrerAccount :: !(Maybe Double) -- ^ "referrerAccount"
    } deriving (Show, Eq, Generic)

instance FromJSON RespAffiliate

data RespAnnouncement = RespAnnouncement
    { id      :: !Double -- ^ /Required/ "id"
    , link    :: !(Maybe Text) -- ^ "link"
    , title   :: !(Maybe Text) -- ^ "title"
    , content :: !(Maybe Text) -- ^ "content"
    , date    :: !(Maybe DateTime) -- ^ "date"
    } deriving (Show, Eq, Generic)

instance FromJSON RespAnnouncement

data RespChat = RespChat
    { id        :: !(Maybe Double) -- ^ "id"
    , date      :: !DateTime -- ^ /Required/ "date"
    , user      :: !Text -- ^ /Required/ "user"
    , message   :: !Text -- ^ /Required/ "message"
    , html      :: !Text -- ^ /Required/ "html"
    , fromBot   :: !(Maybe Bool) -- ^ "fromBot"
    , channelID :: !(Maybe Double) -- ^ "channelID"
    } deriving (Show, Eq, Generic)

instance FromJSON RespChat

data RespConnectedUsers = RespConnectedUsers
    { users :: !(Maybe Int) -- ^ "users"
    , bots  :: !(Maybe Int) -- ^ "bots"
    } deriving (Show, Eq, Generic)

instance FromJSON RespConnectedUsers

data RespExecution = RespExecution
    { execID                :: !Text -- ^ /Required/ "execID"
    , orderID               :: !(Maybe Text) -- ^ "orderID"
    , clOrdID               :: !(Maybe Text) -- ^ "clOrdID"
    , clOrdLinkID           :: !(Maybe Text) -- ^ "clOrdLinkID"
    , account               :: !(Maybe Double) -- ^ "account"
    , symbol                :: !(Maybe Symbol) -- ^ "symbol"
    , side                  :: !(Maybe Side) -- ^ "side"
    , lastQty               :: !(Maybe Double) -- ^ "lastQty"
    , lastPx                :: !(Maybe Double) -- ^ "lastPx"
    , underlyingLastPx      :: !(Maybe Double) -- ^ "underlyingLastPx"
    , lastMkt               :: !(Maybe Text) -- ^ "lastMkt"
    , lastLiquidityInd      :: !(Maybe Text) -- ^ "lastLiquidityInd"
    , simpleOrderQty        :: !(Maybe Double) -- ^ "simpleOrderQty"
    , orderQty              :: !(Maybe Double) -- ^ "orderQty"
    , price                 :: !(Maybe Double) -- ^ "price"
    , displayQty            :: !(Maybe Double) -- ^ "displayQty"
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
    , leavesQty             :: !(Maybe Double) -- ^ "leavesQty"
    , simpleCumQty          :: !(Maybe Double) -- ^ "simpleCumQty"
    , cumQty                :: !(Maybe Double) -- ^ "cumQty"
    , avgPx                 :: !(Maybe Double) -- ^ "avgPx"
    , commission            :: !(Maybe Double) -- ^ "commission"
    , tradePublishIndicator :: !(Maybe Text) -- ^ "tradePublishIndicator"
    , multiLegReportingType :: !(Maybe Text) -- ^ "multiLegReportingType"
    , text                  :: !(Maybe Text) -- ^ "text"
    , trdMatchID            :: !(Maybe Text) -- ^ "trdMatchID"
    , execCost              :: !(Maybe Double) -- ^ "execCost"
    , execComm              :: !(Maybe Double) -- ^ "execComm"
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

data RespInstrument = RespInstrument
    { symbol                         :: !Symbol -- ^ /Required/ "symbol"
    , rootSymbol                     :: !(Maybe Currency) -- ^ "rootSymbol"
    , state                          :: !(Maybe Text) -- ^ "state"
    , typ                            :: !(Maybe Text) -- ^ "typ"
    , listing                        :: !(Maybe DateTime) -- ^ "listing"
    , front                          :: !(Maybe DateTime) -- ^ "front"
    , expiry                         :: !(Maybe DateTime) -- ^ "expiry"
    , settle                         :: !(Maybe DateTime) -- ^ "settle"
    , relistInterval                 :: !(Maybe DateTime) -- ^ "relistInterval"
    , inverseLeg                     :: !(Maybe Text) -- ^ "inverseLeg"
    , sellLeg                        :: !(Maybe Text) -- ^ "sellLeg"
    , buyLeg                         :: !(Maybe Text) -- ^ "buyLeg"
    , positionCurrency               :: !(Maybe Currency) -- ^ "positionCurrency"
    , underlying                     :: !(Maybe Currency) -- ^ "underlying"
    , quoteCurrency                  :: !(Maybe Currency) -- ^ "quoteCurrency"
    , underlyingSymbol               :: !(Maybe Text) -- ^ "underlyingSymbol"
    , reference                      :: !(Maybe Text) -- ^ "reference"
    , referenceSymbol                :: !(Maybe Text) -- ^ "referenceSymbol"
    , calcInterval                   :: !(Maybe DateTime) -- ^ "calcInterval"
    , publishInterval                :: !(Maybe DateTime) -- ^ "publishInterval"
    , publishTime                    :: !(Maybe DateTime) -- ^ "publishTime"
    , maxOrderQty                    :: !(Maybe Double) -- ^ "maxOrderQty"
    , maxPrice                       :: !(Maybe Double) -- ^ "maxPrice"
    , lotSize                        :: !(Maybe Double) -- ^ "lotSize"
    , tickSize                       :: !(Maybe Double) -- ^ "tickSize"
    , multiplier                     :: !(Maybe Double) -- ^ "multiplier"
    , settlCurrency                  :: !(Maybe Currency) -- ^ "settlCurrency"
    , underlyingToPositionMultiplier :: !(Maybe Double) -- ^ "underlyingToPositionMultiplier"
    , underlyingToSettleMultiplier   :: !(Maybe Double) -- ^ "underlyingToSettleMultiplier"
    , quoteToSettleMultiplier        :: !(Maybe Double) -- ^ "quoteToSettleMultiplier"
    , isQuanto                       :: !(Maybe Bool) -- ^ "isQuanto"
    , isInverse                      :: !(Maybe Bool) -- ^ "isInverse"
    , initMargin                     :: !(Maybe Double) -- ^ "initMargin"
    , maintMargin                    :: !(Maybe Double) -- ^ "maintMargin"
    , riskLimit                      :: !(Maybe Double) -- ^ "riskLimit"
    , riskStep                       :: !(Maybe Double) -- ^ "riskStep"
    , limit                          :: !(Maybe Double) -- ^ "limit"
    , capped                         :: !(Maybe Bool) -- ^ "capped"
    , taxed                          :: !(Maybe Bool) -- ^ "taxed"
    , deleverage                     :: !(Maybe Bool) -- ^ "deleverage"
    , makerFee                       :: !(Maybe Double) -- ^ "makerFee"
    , takerFee                       :: !(Maybe Double) -- ^ "takerFee"
    , settlementFee                  :: !(Maybe Double) -- ^ "settlementFee"
    , insuranceFee                   :: !(Maybe Double) -- ^ "insuranceFee"
    , fundingBaseSymbol              :: !(Maybe Text) -- ^ "fundingBaseSymbol"
    , fundingQuoteSymbol             :: !(Maybe Text) -- ^ "fundingQuoteSymbol"
    , fundingPremiumSymbol           :: !(Maybe Text) -- ^ "fundingPremiumSymbol"
    , fundingTimestamp               :: !(Maybe DateTime) -- ^ "fundingTimestamp"
    , fundingInterval                :: !(Maybe DateTime) -- ^ "fundingInterval"
    , fundingRate                    :: !(Maybe Double) -- ^ "fundingRate"
    , indicativeFundingRate          :: !(Maybe Double) -- ^ "indicativeFundingRate"
    , rebalanceTimestamp             :: !(Maybe DateTime) -- ^ "rebalanceTimestamp"
    , rebalanceInterval              :: !(Maybe DateTime) -- ^ "rebalanceInterval"
    , openingTimestamp               :: !(Maybe DateTime) -- ^ "openingTimestamp"
    , closingTimestamp               :: !(Maybe DateTime) -- ^ "closingTimestamp"
    , sessionInterval                :: !(Maybe DateTime) -- ^ "sessionInterval"
    , prevClosePrice                 :: !(Maybe Double) -- ^ "prevClosePrice"
    , limitDownPrice                 :: !(Maybe Double) -- ^ "limitDownPrice"
    , limitUpPrice                   :: !(Maybe Double) -- ^ "limitUpPrice"
    , bankruptLimitDownPrice         :: !(Maybe Double) -- ^ "bankruptLimitDownPrice"
    , bankruptLimitUpPrice           :: !(Maybe Double) -- ^ "bankruptLimitUpPrice"
    , prevTotalVolume                :: !(Maybe Double) -- ^ "prevTotalVolume"
    , totalVolume                    :: !(Maybe Double) -- ^ "totalVolume"
    , volume                         :: !(Maybe Double) -- ^ "volume"
    , volume24h                      :: !(Maybe Double) -- ^ "volume24h"
    , prevTotalTurnover              :: !(Maybe Double) -- ^ "prevTotalTurnover"
    , totalTurnover                  :: !(Maybe Double) -- ^ "totalTurnover"
    , turnover                       :: !(Maybe Double) -- ^ "turnover"
    , turnover24h                    :: !(Maybe Double) -- ^ "turnover24h"
    , prevPrice24h                   :: !(Maybe Double) -- ^ "prevPrice24h"
    , vwap                           :: !(Maybe Double) -- ^ "vwap"
    , highPrice                      :: !(Maybe Double) -- ^ "highPrice"
    , lowPrice                       :: !(Maybe Double) -- ^ "lowPrice"
    , lastPrice                      :: !(Maybe Double) -- ^ "lastPrice"
    , lastPriceProtected             :: !(Maybe Double) -- ^ "lastPriceProtected"
    , lastTickDirection              :: !(Maybe Text) -- ^ "lastTickDirection"
    , lastChangePcnt                 :: !(Maybe Double) -- ^ "lastChangePcnt"
    , bidPrice                       :: !(Maybe Double) -- ^ "bidPrice"
    , midPrice                       :: !(Maybe Double) -- ^ "midPrice"
    , askPrice                       :: !(Maybe Double) -- ^ "askPrice"
    , impactBidPrice                 :: !(Maybe Double) -- ^ "impactBidPrice"
    , impactMidPrice                 :: !(Maybe Double) -- ^ "impactMidPrice"
    , impactAskPrice                 :: !(Maybe Double) -- ^ "impactAskPrice"
    , hasLiquidity                   :: !(Maybe Bool) -- ^ "hasLiquidity"
    , openInterest                   :: !(Maybe Double) -- ^ "openInterest"
    , openValue                      :: !(Maybe Double) -- ^ "openValue"
    , fairMethod                     :: !(Maybe Text) -- ^ "fairMethod"
    , fairBasisRate                  :: !(Maybe Double) -- ^ "fairBasisRate"
    , fairBasis                      :: !(Maybe Double) -- ^ "fairBasis"
    , fairPrice                      :: !(Maybe Double) -- ^ "fairPrice"
    , markMethod                     :: !(Maybe Text) -- ^ "markMethod"
    , markPrice                      :: !(Maybe Double) -- ^ "markPrice"
    , indicativeTaxRate              :: !(Maybe Double) -- ^ "indicativeTaxRate"
    , indicativeSettlePrice          :: !(Maybe Double) -- ^ "indicativeSettlePrice"
    , settledPrice                   :: !(Maybe Double) -- ^ "settledPrice"
    , timestamp                      :: !(Maybe DateTime) -- ^ "timestamp"
    } deriving (Show, Eq, Generic)

instance FromJSON RespInstrument

data RespInsurance = RespInsurance
    { currency      :: !Currency -- ^ /Required/ "currency"
    , timestamp     :: !DateTime -- ^ /Required/ "timestamp"
    , walletBalance :: !(Maybe Double) -- ^ "walletBalance"
    } deriving (Show, Eq, Generic)

instance FromJSON RespInsurance

data RespLiquidation = RespLiquidation
    { orderID   :: !Text -- ^ /Required/ "orderID"
    , symbol    :: !(Maybe Symbol) -- ^ "symbol"
    , side      :: !(Maybe Side) -- ^ "side"
    , price     :: !(Maybe Double) -- ^ "price"
    , leavesQty :: !(Maybe Double) -- ^ "leavesQty"
    } deriving (Show, Eq, Generic)

instance FromJSON RespLiquidation

data RespMargin = RespMargin
    { account            :: !Double -- ^ /Required/ "account"
    , currency           :: !Currency -- ^ /Required/ "currency"
    , riskLimit          :: !(Maybe Double) -- ^ "riskLimit"
    , prevState          :: !(Maybe Text) -- ^ "prevState"
    , state              :: !(Maybe Text) -- ^ "state"
    , action             :: !(Maybe Text) -- ^ "action"
    , amount             :: !(Maybe Double) -- ^ "amount"
    , pendingCredit      :: !(Maybe Double) -- ^ "pendingCredit"
    , pendingDebit       :: !(Maybe Double) -- ^ "pendingDebit"
    , confirmedDebit     :: !(Maybe Double) -- ^ "confirmedDebit"
    , prevRealisedPnl    :: !(Maybe Double) -- ^ "prevRealisedPnl"
    , prevUnrealisedPnl  :: !(Maybe Double) -- ^ "prevUnrealisedPnl"
    , grossComm          :: !(Maybe Double) -- ^ "grossComm"
    , grossOpenCost      :: !(Maybe Double) -- ^ "grossOpenCost"
    , grossOpenPremium   :: !(Maybe Double) -- ^ "grossOpenPremium"
    , grossExecCost      :: !(Maybe Double) -- ^ "grossExecCost"
    , grossMarkValue     :: !(Maybe Double) -- ^ "grossMarkValue"
    , riskValue          :: !(Maybe Double) -- ^ "riskValue"
    , taxableMargin      :: !(Maybe Double) -- ^ "taxableMargin"
    , initMargin         :: !(Maybe Double) -- ^ "initMargin"
    , maintMargin        :: !(Maybe Double) -- ^ "maintMargin"
    , sessionMargin      :: !(Maybe Double) -- ^ "sessionMargin"
    , targetExcessMargin :: !(Maybe Double) -- ^ "targetExcessMargin"
    , varMargin          :: !(Maybe Double) -- ^ "varMargin"
    , realisedPnl        :: !(Maybe Double) -- ^ "realisedPnl"
    , unrealisedPnl      :: !(Maybe Double) -- ^ "unrealisedPnl"
    , indicativeTax      :: !(Maybe Double) -- ^ "indicativeTax"
    , unrealisedProfit   :: !(Maybe Double) -- ^ "unrealisedProfit"
    , syntheticMargin    :: !(Maybe Double) -- ^ "syntheticMargin"
    , walletBalance      :: !(Maybe Double) -- ^ "walletBalance"
    , marginBalance      :: !(Maybe Double) -- ^ "marginBalance"
    , marginBalancePcnt  :: !(Maybe Double) -- ^ "marginBalancePcnt"
    , marginLeverage     :: !(Maybe Double) -- ^ "marginLeverage"
    , marginUsedPcnt     :: !(Maybe Double) -- ^ "marginUsedPcnt"
    , excessMargin       :: !(Maybe Double) -- ^ "excessMargin"
    , excessMarginPcnt   :: !(Maybe Double) -- ^ "excessMarginPcnt"
    , availableMargin    :: !(Maybe Double) -- ^ "availableMargin"
    , withdrawableMargin :: !(Maybe Double) -- ^ "withdrawableMargin"
    , timestamp          :: !(Maybe DateTime) -- ^ "timestamp"
    , grossLastValue     :: !(Maybe Double) -- ^ "grossLastValue"
    , commission         :: !(Maybe Double) -- ^ "commission"
    } deriving (Show, Eq, Generic)

instance FromJSON RespMargin

data RespNotification = RespNotification
    { _id                :: !(Maybe Int) -- ^ "id"
    , _date              :: !DateTime -- ^ /Required/ "date"
    , _title             :: !Text -- ^ /Required/ "title"
    , _body              :: !Text -- ^ /Required/ "body"
    , _ttl               :: !Int -- ^ /Required/ "ttl"
    , _type              :: !(Maybe NotificationType) -- ^ "type"
    , _closable          :: !(Maybe Bool) -- ^ "closable"
    , _persist           :: !(Maybe Bool) -- ^ "persist"
    , _waitForVisibility :: !(Maybe Bool) -- ^ "waitForVisibility"
    , _sound             :: !(Maybe Text) -- ^ "sound"
    } deriving (Show, Eq, Generic)

instance FromJSON RespNotification where
    parseJSON = genericParseJSON opts
      where
        opts = defaultOptions {fieldLabelModifier = drop 1}

data RespOrder = RespOrder
    { orderID               :: !Text -- ^ /Required/ "orderID"
    , clOrdID               :: !(Maybe Text) -- ^ "clOrdID"
    , clOrdLinkID           :: !(Maybe Text) -- ^ "clOrdLinkID"
    , account               :: !(Maybe Double) -- ^ "account"
    , symbol                :: !(Maybe Symbol) -- ^ "symbol"
    , side                  :: !(Maybe Side) -- ^ "side"
    , simpleOrderQty        :: !(Maybe Double) -- ^ "simpleOrderQty"
    , orderQty              :: !(Maybe Double) -- ^ "orderQty"
    , price                 :: !(Maybe Double) -- ^ "price"
    , displayQty            :: !(Maybe Double) -- ^ "displayQty"
    , stopPx                :: !(Maybe Double) -- ^ "stopPx"
    , pegOffsetValue        :: !(Maybe Double) -- ^ "pegOffsetValue"
    , pegPriceType          :: !(Maybe Text) -- ^ "pegPriceType"
    , currency              :: !(Maybe Currency) -- ^ "currency"
    , settlCurrency         :: !(Maybe Currency) -- ^ "settlCurrency"
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
    , leavesQty             :: !(Maybe Double) -- ^ "leavesQty"
    , simpleCumQty          :: !(Maybe Double) -- ^ "simpleCumQty"
    , cumQty                :: !(Maybe Double) -- ^ "cumQty"
    , avgPx                 :: !(Maybe Double) -- ^ "avgPx"
    , multiLegReportingType :: !(Maybe Text) -- ^ "multiLegReportingType"
    , text                  :: !(Maybe Text) -- ^ "text"
    , transactTime          :: !(Maybe DateTime) -- ^ "transactTime"
    , timestamp             :: !(Maybe DateTime) -- ^ "timestamp"
    } deriving (Show, Eq, Generic)

instance FromJSON RespOrder

data RespOrderBookL2 = RespOrderBookL2
    { symbol :: !Symbol -- ^ /Required/ "symbol"
    , id     :: !Double -- ^ /Required/ "id"
    , side   :: !Side -- ^ /Required/ "side"
    , size   :: !(Maybe Double) -- ^ "size"
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

data RespPosition = RespPosition
    { account              :: !Double -- ^ /Required/ "account"
    , symbol               :: !Symbol -- ^ /Required/ "symbol"
    , currency             :: !Currency -- ^ /Required/ "currency"
    , underlying           :: !(Maybe Currency) -- ^ "underlying"
    , quoteCurrency        :: !(Maybe Currency) -- ^ "quoteCurrency"
    , commission           :: !(Maybe Double) -- ^ "commission"
    , initMarginReq        :: !(Maybe Double) -- ^ "initMarginReq"
    , maintMarginReq       :: !(Maybe Double) -- ^ "maintMarginReq"
    , riskLimit            :: !(Maybe Double) -- ^ "riskLimit"
    , leverage             :: !(Maybe Double) -- ^ "leverage"
    , crossMargin          :: !(Maybe Bool) -- ^ "crossMargin"
    , deleveragePercentile :: !(Maybe Double) -- ^ "deleveragePercentile"
    , rebalancedPnl        :: !(Maybe Double) -- ^ "rebalancedPnl"
    , prevRealisedPnl      :: !(Maybe Double) -- ^ "prevRealisedPnl"
    , prevUnrealisedPnl    :: !(Maybe Double) -- ^ "prevUnrealisedPnl"
    , prevClosePrice       :: !(Maybe Double) -- ^ "prevClosePrice"
    , openingTimestamp     :: !(Maybe DateTime) -- ^ "openingTimestamp"
    , openingQty           :: !(Maybe Double) -- ^ "openingQty"
    , openingCost          :: !(Maybe Double) -- ^ "openingCost"
    , openingComm          :: !(Maybe Double) -- ^ "openingComm"
    , openOrderBuyQty      :: !(Maybe Double) -- ^ "openOrderBuyQty"
    , openOrderBuyCost     :: !(Maybe Double) -- ^ "openOrderBuyCost"
    , openOrderBuyPremium  :: !(Maybe Double) -- ^ "openOrderBuyPremium"
    , openOrderSellQty     :: !(Maybe Double) -- ^ "openOrderSellQty"
    , openOrderSellCost    :: !(Maybe Double) -- ^ "openOrderSellCost"
    , openOrderSellPremium :: !(Maybe Double) -- ^ "openOrderSellPremium"
    , execBuyQty           :: !(Maybe Double) -- ^ "execBuyQty"
    , execBuyCost          :: !(Maybe Double) -- ^ "execBuyCost"
    , execSellQty          :: !(Maybe Double) -- ^ "execSellQty"
    , execSellCost         :: !(Maybe Double) -- ^ "execSellCost"
    , execQty              :: !(Maybe Double) -- ^ "execQty"
    , execCost             :: !(Maybe Double) -- ^ "execCost"
    , execComm             :: !(Maybe Double) -- ^ "execComm"
    , currentTimestamp     :: !(Maybe DateTime) -- ^ "currentTimestamp"
    , currentQty           :: !(Maybe Double) -- ^ "currentQty"
    , currentCost          :: !(Maybe Double) -- ^ "currentCost"
    , currentComm          :: !(Maybe Double) -- ^ "currentComm"
    , realisedCost         :: !(Maybe Double) -- ^ "realisedCost"
    , unrealisedCost       :: !(Maybe Double) -- ^ "unrealisedCost"
    , grossOpenCost        :: !(Maybe Double) -- ^ "grossOpenCost"
    , grossOpenPremium     :: !(Maybe Double) -- ^ "grossOpenPremium"
    , grossExecCost        :: !(Maybe Double) -- ^ "grossExecCost"
    , isOpen               :: !(Maybe Bool) -- ^ "isOpen"
    , markPrice            :: !(Maybe Double) -- ^ "markPrice"
    , markValue            :: !(Maybe Double) -- ^ "markValue"
    , riskValue            :: !(Maybe Double) -- ^ "riskValue"
    , homeNotional         :: !(Maybe Double) -- ^ "homeNotional"
    , foreignNotional      :: !(Maybe Double) -- ^ "foreignNotional"
    , posState             :: !(Maybe Text) -- ^ "posState"
    , posCost              :: !(Maybe Double) -- ^ "posCost"
    , posCost2             :: !(Maybe Double) -- ^ "posCost2"
    , posCross             :: !(Maybe Double) -- ^ "posCross"
    , posInit              :: !(Maybe Double) -- ^ "posInit"
    , posComm              :: !(Maybe Double) -- ^ "posComm"
    , posLoss              :: !(Maybe Double) -- ^ "posLoss"
    , posMargin            :: !(Maybe Double) -- ^ "posMargin"
    , posMaint             :: !(Maybe Double) -- ^ "posMaint"
    , posAllowance         :: !(Maybe Double) -- ^ "posAllowance"
    , taxableMargin        :: !(Maybe Double) -- ^ "taxableMargin"
    , initMargin           :: !(Maybe Double) -- ^ "initMargin"
    , maintMargin          :: !(Maybe Double) -- ^ "maintMargin"
    , sessionMargin        :: !(Maybe Double) -- ^ "sessionMargin"
    , targetExcessMargin   :: !(Maybe Double) -- ^ "targetExcessMargin"
    , varMargin            :: !(Maybe Double) -- ^ "varMargin"
    , realisedGrossPnl     :: !(Maybe Double) -- ^ "realisedGrossPnl"
    , realisedTax          :: !(Maybe Double) -- ^ "realisedTax"
    , realisedPnl          :: !(Maybe Double) -- ^ "realisedPnl"
    , unrealisedGrossPnl   :: !(Maybe Double) -- ^ "unrealisedGrossPnl"
    , longBankrupt         :: !(Maybe Double) -- ^ "longBankrupt"
    , shortBankrupt        :: !(Maybe Double) -- ^ "shortBankrupt"
    , taxBase              :: !(Maybe Double) -- ^ "taxBase"
    , indicativeTaxRate    :: !(Maybe Double) -- ^ "indicativeTaxRate"
    , indicativeTax        :: !(Maybe Double) -- ^ "indicativeTax"
    , unrealisedTax        :: !(Maybe Double) -- ^ "unrealisedTax"
    , unrealisedPnl        :: !(Maybe Double) -- ^ "unrealisedPnl"
    , unrealisedPnlPcnt    :: !(Maybe Double) -- ^ "unrealisedPnlPcnt"
    , unrealisedRoePcnt    :: !(Maybe Double) -- ^ "unrealisedRoePcnt"
    , simpleQty            :: !(Maybe Double) -- ^ "simpleQty"
    , simpleCost           :: !(Maybe Double) -- ^ "simpleCost"
    , simpleValue          :: !(Maybe Double) -- ^ "simpleValue"
    , simplePnl            :: !(Maybe Double) -- ^ "simplePnl"
    , simplePnlPcnt        :: !(Maybe Double) -- ^ "simplePnlPcnt"
    , avgCostPrice         :: !(Maybe Double) -- ^ "avgCostPrice"
    , avgEntryPrice        :: !(Maybe Double) -- ^ "avgEntryPrice"
    , breakEvenPrice       :: !(Maybe Double) -- ^ "breakEvenPrice"
    , marginCallPrice      :: !(Maybe Double) -- ^ "marginCallPrice"
    , liquidationPrice     :: !(Maybe Double) -- ^ "liquidationPrice"
    , bankruptPrice        :: !(Maybe Double) -- ^ "bankruptPrice"
    , timestamp            :: !(Maybe DateTime) -- ^ "timestamp"
    , lastPrice            :: !(Maybe Double) -- ^ "lastPrice"
    , lastValue            :: !(Maybe Double) -- ^ "lastValue"
    } deriving (Show, Eq, Generic)

instance FromJSON RespPosition

data RespQuote = RespQuote
    { timestamp :: !DateTime -- ^ /Required/ "timestamp"
    , symbol    :: !Symbol -- ^ /Required/ "symbol"
    , bidSize   :: !(Maybe Double) -- ^ "bidSize"
    , bidPrice  :: !(Maybe Double) -- ^ "bidPrice"
    , askPrice  :: !(Maybe Double) -- ^ "askPrice"
    , askSize   :: !(Maybe Double) -- ^ "askSize"
    } deriving (Show, Eq, Generic)

instance FromJSON RespQuote

data RespSettlement = RespSettlement
    { timestamp      :: !DateTime -- ^ /Required/ "timestamp"
    , symbol         :: !Symbol -- ^ /Required/ "symbol"
    , settlementType :: !(Maybe Text) -- ^ "settlementType"
    , settledPrice   :: !(Maybe Double) -- ^ "settledPrice"
    , bankrupt       :: !(Maybe Double) -- ^ "bankrupt"
    , taxBase        :: !(Maybe Double) -- ^ "taxBase"
    , taxRate        :: !(Maybe Double) -- ^ "taxRate"
    } deriving (Show, Eq, Generic)

instance FromJSON RespSettlement

data RespTrade = RespTrade
    { timestamp       :: !DateTime -- ^ /Required/ "timestamp"
    , symbol          :: !Symbol -- ^ /Required/ "symbol"
    , side            :: !(Maybe Side) -- ^ "side"
    , size            :: !(Maybe Double) -- ^ "size"
    , price           :: !(Maybe Double) -- ^ "price"
    , tickDirection   :: !(Maybe Text) -- ^ "tickDirection"
    , trdMatchID      :: !(Maybe Text) -- ^ "trdMatchID"
    , grossValue      :: !(Maybe Double) -- ^ "grossValue"
    , homeNotional    :: !(Maybe Double) -- ^ "homeNotional"
    , foreignNotional :: !(Maybe Double) -- ^ "foreignNotional"
    } deriving (Show, Eq, Generic)

instance FromJSON RespTrade

-- TODO: TradeBin
data RespTransaction = RespTransaction
    { transactID     :: !Text -- ^ /Required/ "transactID"
    , account        :: !(Maybe Double) -- ^ "account"
    , currency       :: !(Maybe Currency) -- ^ "currency"
    , transactType   :: !(Maybe Text) -- ^ "transactType"
    , amount         :: !(Maybe Double) -- ^ "amount"
    , fee            :: !(Maybe Double) -- ^ "fee"
    , transactStatus :: !(Maybe Text) -- ^ "transactStatus"
    , address        :: !(Maybe Text) -- ^ "address"
    , tx             :: !(Maybe Text) -- ^ "tx"
    , text           :: !(Maybe Text) -- ^ "text"
    , transactTime   :: !(Maybe DateTime) -- ^ "transactTime"
    , timestamp      :: !(Maybe DateTime) -- ^ "timestamp"
    } deriving (Show, Eq, Generic)

instance FromJSON RespTransaction

data RespWallet = RespWallet
    { account          :: !Double -- ^ /Required/ "account"
    , currency         :: !Currency -- ^ /Required/ "currency"
    , prevDeposited    :: !(Maybe Double) -- ^ "prevDeposited"
    , prevWithdrawn    :: !(Maybe Double) -- ^ "prevWithdrawn"
    , prevTransferIn   :: !(Maybe Double) -- ^ "prevTransferIn"
    , prevTransferOut  :: !(Maybe Double) -- ^ "prevTransferOut"
    , prevAmount       :: !(Maybe Double) -- ^ "prevAmount"
    , prevTimestamp    :: !(Maybe DateTime) -- ^ "prevTimestamp"
    , deltaDeposited   :: !(Maybe Double) -- ^ "deltaDeposited"
    , deltaWithdrawn   :: !(Maybe Double) -- ^ "deltaWithdrawn"
    , deltaTransferIn  :: !(Maybe Double) -- ^ "deltaTransferIn"
    , deltaTransferOut :: !(Maybe Double) -- ^ "deltaTransferOut"
    , deltaAmount      :: !(Maybe Double) -- ^ "deltaAmount"
    , deposited        :: !(Maybe Double) -- ^ "deposited"
    , withdrawn        :: !(Maybe Double) -- ^ "withdrawn"
    , transferIn       :: !(Maybe Double) -- ^ "transferIn"
    , transferOut      :: !(Maybe Double) -- ^ "transferOut"
    , amount           :: !(Maybe Double) -- ^ "amount"
    , pendingCredit    :: !(Maybe Double) -- ^ "pendingCredit"
    , pendingDebit     :: !(Maybe Double) -- ^ "pendingDebit"
    , confirmedDebit   :: !(Maybe Double) -- ^ "confirmedDebit"
    , timestamp        :: !(Maybe DateTime) -- ^ "timestamp"
    , addr             :: !(Maybe Text) -- ^ "addr"
    , script           :: !(Maybe Text) -- ^ "script"
    , withdrawalLock   :: !(Maybe (Vector Text)) -- ^ "withdrawalLock"
    } deriving (Show, Eq, Generic)

instance FromJSON RespWallet

data Response
    = Aff (TABLE RespAffiliate)
    | Ann (TABLE RespAnnouncement)
    | C (TABLE RespChat)
    | CU (TABLE RespConnectedUsers)
    | Exe (TABLE RespExecution)
    | F (TABLE RespFunding)
    | I (TABLE RespInstrument)
    | Insu (TABLE RespInsurance)
    | L (TABLE RespLiquidation)
    | M (TABLE RespMargin)
    | N (TABLE RespNotification)
    | O (TABLE RespOrder)
    | OB (TABLE RespOrderBookL2)
    | OB10 (TABLE RespOrderBook10)
    | P (TABLE RespPosition)
    | Q (TABLE RespQuote)
    | Setl (TABLE RespSettlement)
    | T (TABLE RespTrade)
    -- | TB (TABLE RespTradeBin)
    | TX (TABLE RespTransaction)
    | W (TABLE RespWallet)
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
                Just _ ->
                    fail
                        "Cannot parse response: the kind of the response is not supported"
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
                                                "Cannot parse response: unknown response format"
      where
        opts =
            defaultOptions
            { fieldLabelModifier = drop 1
            , sumEncoding = UntaggedValue
            }
