{-# LANGUAGE RankNTypes #-}

module BitMEXWrapper.Types
    ( BitMEXWrapperConfig(..)
    , BitMEXReader(..)
    , Environment(..)
    , BitMEXApp
    ) where

import           BitMEX
    ( LogContext
    , LogExecWithContext
    )
import           Control.Monad.Reader
    ( Monad
    , MonadIO
    , MonadReader
    , ReaderT
    , asks
    , local
    )
import qualified Data.ByteString      as SBS (ByteString)
import qualified Data.ByteString.Lazy as LBS (ByteString)
import           Data.Text            (Text)
import           Network.HTTP.Client  (Manager)
import           Network.WebSockets   (Connection)
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
    { environment    :: !Environment
    , pathREST       :: !(Maybe LBS.ByteString)
    , pathWS         :: !(Maybe LBS.ByteString)
    , manager        :: !(Maybe Manager)
    , publicKey      :: !Text
    , privateKey     :: !SBS.ByteString
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

-- instance K.Katip BitMEXReader where
--     getLogEnv = asks logEnv
--     localLogEnv f (BitMEXReader m) =
--         BitMEXReader
--             (local (\s -> s {logEnv = f (logEnv s)}) m)

-- instance K.KatipContext BitMEXReader where
--     getKatipContext = asks logContext
--     localKatipContext f (BitMEXReader m) =
--         BitMEXReader
--             (local
--                  (\s -> s {logContext = f (logContext s)})
--                  m)
--     getKatipNamespace = asks logNamespace
--     localKatipNamespace f (BitMEXReader m) =
--         BitMEXReader
--             (local
--                  (\s ->
--                       s {logNamespace = f (logNamespace s)})
--                  m)
