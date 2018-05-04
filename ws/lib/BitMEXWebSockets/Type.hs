module BitMEXWebSockets.Type
    ( Command(..)
    , Topic(..)
    , Message(..)
    ) where

import           Data.Aeson
    ( ToJSON
    , constructorTagModifier
    , defaultOptions
    , genericToJSON
    , toJSON
    )
import           Data.Char    (toLower)
import           GHC.Generics
import           Prelude      (Eq, Show, drop)

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

data Topic
    = Announcement
    | Chat
    | Connected
    | Funding
    | Instrument
    | Insurance
    | Liquidation
    | OrderBookL2
    | OrderBook10
    | PublicNotifications
    | Quote
    | QuoteBin1m
    | QuoteBin5m
    | QuoteBin1h
    | QuoteBin1d
    | Settlement
    | Trade
    | TradeBin1m
    | TradeBin5m
    | TradeBin1h
    | TradeBin1d
    | Affiliate
    | Execution
    | Order
    | Margin
    | Position
    | PrivateNotifications
    | Transact
    | Wallet
    deriving (Eq, Show, Generic)

instance ToJSON Topic where
    toJSON = genericToJSON opts
      where
        opts =
            defaultOptions
            { constructorTagModifier =
                  (\(x:xs) -> (toLower x : xs))
            }

data Message = Message
    { op   :: !Command
    , args :: [Topic]
    } deriving (Eq, Show, Generic)

instance ToJSON Message
