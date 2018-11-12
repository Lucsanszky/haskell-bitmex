{-# LANGUAGE UndecidableInstances #-}

module BitMEXClient.Wrapper.Types
    ( BitMEXWrapperConfig(..)
    , BitMEXReader(..)
    , Environment(..)
    , BitMEXApp
    -- , Authenticator(..)
    , run
    ) where

import           BitMEX
    ( AuthApiKeyApiKey (..)
    , AuthApiKeyApiNonce (..)
    , AuthApiKeyApiSignature (..)
    , BitMEXConfig (..)
    , LogContext
    , addAuthMethod
    )
import           BitMEXClient.CustomPrelude
import           Data.ByteArray
    ( ByteArrayAccess
    )
import qualified Data.ByteString            as BS
    ( ByteString
    )
import qualified Data.ByteString.Lazy       as LBS
    ( ByteString
    )
import           Data.Text                  (Text)
import qualified Data.Text                  as T (pack)

data Environment
    = MainNet
    | TestNet
    deriving (Eq)

instance Show Environment where
    show MainNet = "https://www.bitmex.com"
    show TestNet = "https://testnet.bitmex.com"

-- class Monad m => Authenticator m where
--     authenticate :: (ByteArrayAccess a) => BitMEXConfig -> a -> m BitMEXConfig

-- newtype AuthReader m a = AuthReader (m a)
--     deriving (Functor, Applicative, Monad)

-- instance ( HasReader "publicKey" Text m
--          , HasReader "privateKey" BS.ByteString m
--          , HasReader "authenticator" (BitMEXConfig -> BS.ByteString -> IO BitMEXConfig) m
--          , Monad m
--          ) => Authenticator (AuthReader m)
--     where
--         authenticate config msg = coerce @(m BitMEXConfig) $ do
--             pub <- ask @"publicKey"
--             secret <- ask @"privateKey"
--             sig <- return (hmacGetDigest . hmac secret $ msg :: Digest SHA256)
--             return $
--               config `addAuthMethod`
--               AuthApiKeyApiSignature ((T.pack . show) sig) `addAuthMethod`
--               AuthApiKeyApiNonce "" `addAuthMethod`
--               AuthApiKeyApiKey pub

data BitMEXWrapperConfig = BitMEXWrapperConfig
    { environment :: !Environment
    , pathREST    :: !(Maybe LBS.ByteString)
    , pathWS      :: !(Maybe LBS.ByteString)
    , manager     :: !(Maybe Manager)
    , publicKey   :: !Text
    , privateKey  :: !BS.ByteString
    , logContext  :: !LogContext
    } deriving Generic

newtype BitMEXReader a = BitMEXReader
    ( ReaderT BitMEXWrapperConfig IO a
    ) deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               )
      -- deriving Authenticator via AuthReader (Field "authenticator" () (MonadReader (ReaderT BitMEXWrapperConfig IO)))
      deriving (HasReader "environment" Environment) via
          Field "environment" () (MonadReader (ReaderT BitMEXWrapperConfig IO))
      deriving (HasReader "pathREST" (Maybe LBS.ByteString)) via
          Field "pathREST" () (MonadReader (ReaderT BitMEXWrapperConfig IO))
      deriving (HasReader "pathWS" (Maybe LBS.ByteString)) via
          Field "pathWS" () (MonadReader (ReaderT BitMEXWrapperConfig IO))
      deriving (HasReader "manager" (Maybe Manager)) via
          Field "manager" () (MonadReader (ReaderT BitMEXWrapperConfig IO))
      deriving (HasReader "publicKey" Text) via
          Field "publicKey" () (MonadReader (ReaderT BitMEXWrapperConfig IO))
      deriving (HasReader "privateKey" BS.ByteString) via
          Field "privateKey" () (MonadReader (ReaderT BitMEXWrapperConfig IO))
      deriving (HasReader "logContext" LogContext) via
          Field "logContext" () (MonadReader (ReaderT BitMEXWrapperConfig IO))
      deriving (HasReader "config" BitMEXWrapperConfig) via
          MonadReader (ReaderT BitMEXWrapperConfig IO)

run :: BitMEXReader a -> BitMEXWrapperConfig -> IO a
run (BitMEXReader m) = runReaderT m

type BitMEXApp a = Connection -> BitMEXReader a
