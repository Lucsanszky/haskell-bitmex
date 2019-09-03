module BitMEXClient.WebSockets.Types.General
    ( Symbol(..)
    , Currency(..)
    , Side(..)
    , OrderType(..)
    , ExecutionInstruction(..)
    , ContingencyType(..)
    ) where

import           BitMEXClient.CustomPrelude

data Side
    = Buy
    | Sell
    deriving (Eq, Show, Generic)

instance FromJSON Side

instance ToJSON Side

data Currency
    = XBT
    | XBt
    | USD
    | A50
    | ADA
    | BCH
    | BFX
    | BLOCKS
    | BVOL
    | COIN
    | DAO
    | DASH
    | EOS
    | ETC
    | ETH
    | FCT
    | GNO
    | LSK
    | LTC
    | NEO
    | QTUM
    | REP
    | SEGWIT
    | SNT
    | WIN
    | XBC
    | XBJ
    | XBU
    | XLM
    | XLT
    | XMR
    | XRP
    | XTZ
    | ZEC
    | Total
    deriving (Eq, Show, Generic)

instance ToJSON Currency

instance FromJSON Currency

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
    | XBTM19
    | XBTU19
    | XBTZ19
    deriving (Eq, Show, Generic)

instance ToJSON Symbol

instance FromJSON Symbol

data OrderType
    = Market
    | Limit
    | Stop
    | StopLimit
    | MarketIfTouched
    | LimitIfTouched
    | MarketWithLeftOverAsLimit
    | Pegged
    deriving (Eq, Show, Generic)

instance FromJSON OrderType

instance ToJSON OrderType

data ExecutionInstruction
    = ParticipateDoNotInitiate
    | AllOrNone
    | MarkPrice
    | IndexPrice
    | LastPrice
    | Close
    | ReduceOnly
    | Fixed
    deriving (Eq, Show, Generic)

instance FromJSON ExecutionInstruction

instance ToJSON ExecutionInstruction

data ContingencyType
    = OCO -- ^ One Cancels the Other
    | OTO -- ^ One Triggers the Other
    | OUOA -- ^ One Updates the Other Absoulute
    | OUOP -- ^ One Updates the Other Proportional
    deriving (Eq, Generic)

instance Show ContingencyType where
    show OCO  = "OneCancelsTheOther"
    show OTO  = "OneTriggersTheOther"
    show OUOA = "OneUpdatesTheOtherAbsolute"
    show OUOP = "OneUpdatesTheOtherProportional"

instance FromJSON ContingencyType

instance ToJSON ContingencyType
