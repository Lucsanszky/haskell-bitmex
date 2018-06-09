{-# LANGUAGE RankNTypes #-}

module BitMEXWrapper.Wrapper
    ( makeRequest
    , connect
    , sign
    , makeTimestamp
    , getMessage
    , withStdoutLoggingWS
    ) where

import           BitMEX
    ( AuthApiKeyApiKey (..)
    , AuthApiKeyApiNonce (..)
    , AuthApiKeyApiSignature (..)
    , BitMEXConfig (..)
    , BitMEXRequest (..)
    , LogContext
    , LogExec
    , LogExecWithContext
    , MimeResult
    , MimeType
    , MimeUnrender
    , Produces
    , addAuthMethod
    , dispatchMime
    , initLogContext
    , runDefaultLogExecWithContext
    , setHeader
    , withStdoutLogging
    )
import           BitMEX.Logging
import           BitMEXWebSockets.Types     (Response)
import           BitMEXWrapper.Types
import           Control.Exception.Safe     (MonadCatch)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader
    ( asks
    , liftIO
    , runReaderT
    )
import           Crypto.Hash                (Digest)
import           Crypto.Hash.Algorithms     (SHA256)
import           Crypto.MAC.HMAC
    ( hmac
    , hmacGetDigest
    )
import           Data.Aeson                 (decode)
import           Data.ByteArray
    ( ByteArrayAccess
    )
import           Data.ByteString.Char8      (pack)
import           Data.ByteString.Conversion (toByteString')
import           Data.ByteString.Lazy       (append)
import qualified Data.ByteString.Lazy.Char8 as LC
    ( pack
    , unpack
    )
import qualified Data.Text                  as T
    ( Text
    , pack
    )
import           Data.Text.Lazy             (toStrict)
import           Data.Text.Lazy.Encoding    (decodeUtf8)
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Network.Socket             (withSocketsDo)
import           Network.WebSockets
    ( Connection
    , receiveData
    )
import           Prelude
    ( Bool (..)
    , IO ()
    , Int
    , Maybe (..)
    , RealFrac
    , drop
    , filter
    , floor
    , head
    , print
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
import           Wuss
    ( runSecureClient
    )

sign ::
       (ByteArrayAccess a)
    => a
    -> BitMEXReader (Digest SHA256)
sign body = do
    secret <- asks privateKey
    return . hmacGetDigest . hmac secret $ body

makeRESTConfig :: BitMEXReader BitMEXConfig
makeRESTConfig = do
    env <- asks environment
    let base = (LC.pack . show) env
    Just path <- asks pathREST
    logCxt <- asks logContext
    let logExecContext = asks logExecContext
    return
        BitMEXConfig
        { configHost = append base path
        , configUserAgent =
              "swagger-haskell-http-client/1.0.0"
        , configLogExecWithContext = logExecContext
        , configLogContext = logCxt
        , configAuthMethods = []
        , configValidateAuthMethods = True
        }

makeTimestamp :: (RealFrac a) => a -> Int
makeTimestamp = floor . (* 1000)

makeRequest ::
       ( Produces req accept
       , MimeUnrender accept res
       , MimeType contentType
       )
    => BitMEXRequest req contentType res accept
    -> BitMEXReader (MimeResult res)
makeRequest req@BitMEXRequest {..} = do
    Just mgr <- asks manager
    pub <- asks publicKey
    time <- liftIO $ makeTimestamp <$> getPOSIXTime
    config0 <- makeRESTConfig >>= liftIO . withStdoutLogging
    let verb = filter (/= '"') $ show rMethod
    sig <-
        sign
            (pack
                 (verb ++
                  "/api/v1" ++
                  (LC.unpack . head) rUrlPath ++ show time))
    let new =
            setHeader
                req
                [("api-expires", toByteString' time)]
        config =
            config0 `addAuthMethod`
            AuthApiKeyApiSignature ((T.pack . show) sig) `addAuthMethod`
            AuthApiKeyApiNonce "" `addAuthMethod`
            AuthApiKeyApiKey pub
    liftIO $ dispatchMime mgr config new

connect :: BitMEXWrapperConfig -> BitMEXApp () -> IO ()
connect config@BitMEXWrapperConfig {..} app = do
    let base = (drop 8 . show) environment
        Just path = pathWS
    withSocketsDo $
        runSecureClient base 443 (LC.unpack path) $ \conn -> do
            runReaderT (run (app conn)) config

getMessage :: Connection -> BitMEXWrapperConfig -> IO (Maybe Response)
getMessage conn config = do
    msg <- receiveData conn
    runConfigLogWithExceptions "WebSocket" config $ do
        case (decode msg :: Maybe Response) of
            Nothing -> do
                    _log "WebSocket" levelError $ (toStrict . decodeUtf8) msg
                    return Nothing
            Just r -> do
                  _log "WebSocket" levelInfo $ (toStrict . decodeUtf8) msg
                  return (Just r)

withStdoutLoggingWS :: BitMEXWrapperConfig -> IO BitMEXWrapperConfig
withStdoutLoggingWS p = do
    logCxt <- stdoutLoggingContext (logContext p)
    return $ p { logExecContext = stdoutLoggingExec, logContext = logCxt }

runConfigLog
  :: MonadIO m
  => BitMEXWrapperConfig -> LogExec m
runConfigLog config = logExecContext config (logContext config)

runConfigLogWithExceptions
  :: (MonadCatch m, MonadIO m)
  => T.Text -> BitMEXWrapperConfig -> LogExec m
runConfigLogWithExceptions src config = runConfigLog config . logExceptions src
