module BitMEXWebSockets.Types.General
    ( Symbol(..)
    ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics
import           Prelude      (Eq, Show)

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
