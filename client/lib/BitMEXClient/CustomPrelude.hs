module BitMEXClient.CustomPrelude
    ( module X
    ) where

import           Control.Exception.Safe  as X (MonadCatch)
import           Control.Monad           as X (fail)
import           Control.Monad.Reader    as X
    ( Monad
    , MonadIO
    , MonadReader
    , MonadTrans
    , ReaderT
    , asks
    , liftIO
    , runReaderT
    )
import           Crypto.Hash             as X (Digest)
import           Crypto.Hash.Algorithms  as X (SHA256)
import           Crypto.MAC.HMAC         as X
    ( hmac
    , hmacGetDigest
    )
import           Data.Aeson              as X
    ( FromJSON
    , SumEncoding (UntaggedValue)
    , ToJSON
    , Value (..)
    , constructorTagModifier
    , decode
    , defaultOptions
    , encode
    , fieldLabelModifier
    , genericParseJSON
    , genericToJSON
    , parseJSON
    , sumEncoding
    , toJSON
    , withObject
    , (.:?)
    )
import           Data.Char               as X (toLower)
import           Data.Time.Clock.POSIX   as X (getPOSIXTime)
import           GHC.Generics            as X
import           Lens.Micro              as X ((.~), (^.))
import           Network.HTTP.Client     as X
    ( Manager
    , newManager
    )
import           Network.HTTP.Client.TLS as X
    ( tlsManagerSettings
    )
import           Network.Socket          as X
    ( withSocketsDo
    )
import           Network.WebSockets      as X
    ( Connection
    , receiveData
    , sendTextData
    )
import           Prelude                 as X
    ( Applicative
    , Bool (..)
    , Double
    , Eq
    , Functor
    , IO
    , Int
    , Integer
    , Maybe (..)
    , RealFrac
    , Show
    , drop
    , filter
    , floor
    , head
    , map
    , return
    , show
    , ($)
    , (*)
    , (++)
    , (.)
    , (/=)
    , (<$>)
    , (>>=)
    )
import           Wuss                    as X
    ( runSecureClient
    )
