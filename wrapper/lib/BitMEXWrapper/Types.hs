module BitMEXWrapper.Types
    ( BitMEXWrapperConfig(..)
    , BitMEXReader(..)
    , Environment(..)
    ) where

import           Control.Monad.Reader
    ( Monad
    , MonadIO
    , MonadReader
    , ReaderT
    )
import qualified Data.ByteString      as SBS (ByteString)
import qualified Data.ByteString.Lazy as LBS (ByteString)
import           Data.Text            (Text)
import           Network.HTTP.Client  (Manager)
import           Prelude
    ( Applicative
    , Eq
    , Functor
    , IO
    , Maybe
    , Show
    , show
    )

data Environment
    = MainNet
    | TestNet
    deriving (Eq)

instance Show Environment where
    show MainNet = "https://www.bitmex.com"
    show TestNet = "https://testnet.bitmex.com"

data BitMEXWrapperConfig = BitMEXWrapperConfig
    { environment :: !Environment
    , pathREST    :: !(Maybe LBS.ByteString)
    , pathWS      :: !(Maybe LBS.ByteString)
    , manager     :: !(Maybe Manager)
    , publicKey   :: !Text
    , privateKey  :: !SBS.ByteString
    }

newtype BitMEXReader a = BitMEXReader
    { run :: (ReaderT BitMEXWrapperConfig IO) a
    } deriving ( Applicative
               , Functor
               , Monad
               , MonadIO
               , MonadReader BitMEXWrapperConfig
               )
