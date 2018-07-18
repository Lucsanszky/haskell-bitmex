module BitMEXClient.Wrapper.Types
    ( BitMEXWrapperConfig(..)
    , BitMEXReader(..)
    , Environment(..)
    , BitMEXApp
    ) where

import           BitMEX
    ( LogContext
    , LogExecWithContext
    )
import           BitMEXClient.CustomPrelude
import qualified Data.ByteString            as BS
    ( ByteString
    )
import qualified Data.ByteString.Lazy       as LBS
    ( ByteString
    )
import           Data.Text                  (Text)

data Environment
    = MainNet
    | TestNet
    deriving (Eq)

instance Show Environment where
    show MainNet = "https://www.bitmex.com"
    show TestNet = "https://testnet.bitmex.com"

data BitMEXWrapperConfig = BitMEXWrapperConfig
    { environment    :: !Environment
    , pathREST       :: !(Maybe LBS.ByteString)
    , pathWS         :: !(Maybe LBS.ByteString)
    , manager        :: !(Maybe Manager)
    , publicKey      :: !Text
    , privateKey     :: !BS.ByteString
    , logExecContext :: !LogExecWithContext
    , logContext     :: !LogContext
    }

newtype BitMEXReader a = BitMEXReader
    { run :: (ReaderT BitMEXWrapperConfig IO) a
    } deriving ( Applicative
               , Functor
               , Monad
               , MonadIO
               , MonadReader BitMEXWrapperConfig
               )

type BitMEXApp a = Connection -> BitMEXReader a
