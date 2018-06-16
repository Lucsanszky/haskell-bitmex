module BitMEXClient.WebSockets.Types.General
    ( Symbol(..)
    , Currency(..)
    , Side(..)
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
    deriving (Eq, Show, Generic)

instance ToJSON Symbol

instance FromJSON Symbol
