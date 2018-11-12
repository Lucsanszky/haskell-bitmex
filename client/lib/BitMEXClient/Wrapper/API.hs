module BitMEXClient.Wrapper.API
    ( makeRequest
    , connect
    , withConnectAndSubscribe
    , sign
    , makeTimestamp
    , getMessage
    , sendMessage
    ) where

import           BitMEX
    ( AuthApiKeyApiKey (..)
    , AuthApiKeyApiNonce (..)
    , AuthApiKeyApiSignature (..)
    , BitMEXConfig (..)
    , BitMEXRequest (..)
    , MimeResult
    , MimeType
    , MimeUnrender
    , ParamBody (..)
    , Produces
    , addAuthMethod
    , dispatchMime
    , paramsBodyL
    , paramsQueryL
    , setHeader
    )
import           BitMEX.Logging
import           BitMEXClient.CustomPrelude
import           BitMEXClient.WebSockets.Types
    ( Command (..)
    , Message (..)
    , Response (..)
    , Symbol
    , Topic (..)
    )
import           BitMEXClient.Wrapper.Logging
import           BitMEXClient.Wrapper.Types
import           Data.ByteArray
    ( ByteArrayAccess
    )
import qualified Data.ByteString.Char8         as BC
    ( pack
    , unpack
    , ByteString
    )
import           Data.ByteString.Conversion
    ( toByteString'
    )
import qualified Data.ByteString.Lazy          as LBS
    ( append
    , ByteString
    )
import qualified Data.ByteString.Lazy.Char8    as LBC
    ( pack
    , unpack
    )
import Data.Text (Text)
import qualified Data.Text                     as T (pack)
import qualified Data.Text.Lazy                as LT
    ( toStrict
    )
import qualified Data.Text.Lazy.Encoding       as LT
    ( decodeUtf8
    )
import           Data.Vector                   (fromList)

------------------------------------------------------------
-- HELPERS

-- | Create a signature for the request.
sign ::
       ( ByteArrayAccess a
       , HasReader "privateKey" BC.ByteString m
       )
    => a
    -> m (Digest SHA256)
sign body = do
    secret <- ask @"privateKey"
    return . hmacGetDigest . hmac secret $ body

makeRESTConfig ::
       ( HasReader "environment" Environment m
       , HasReader "logContext" LogContext m
       , HasReader "pathREST" (Maybe LBS.ByteString) m
       )
    => m BitMEXConfig
makeRESTConfig = do
    env <- ask @"environment"
    logCxt <- ask @"logContext"
    path <-
        ask @"pathREST" >>= \p ->
            return $
            case p of
                Nothing -> "/api/v1"
                Just x  -> x
    let base = (LBC.pack . show) env
    return
        BitMEXConfig
        { configHost = LBS.append base path
        , configUserAgent =
              "swagger-haskell-http-client/1.0.0"
        , configLogExecWithContext = runDefaultLogExecWithContext
        , configLogContext = logCxt
        , configAuthMethods = []
        , configValidateAuthMethods = True
        }

-- | Convenience function to generate a timestamp
-- for the signature of the request.
makeTimestamp :: (RealFrac a) => a -> Int
makeTimestamp = floor . (* 1000000)

------------------------------------------------------------
-- REST

-- | Prepare, authenticate and dispatch a request
-- via the auto-generated BitMEX REST API.
makeRequest ::
      ( Produces req accept
      , MimeUnrender accept res
      , MimeType contentType
      , HasReader "environment" Environment m
      , HasReader "pathREST" (Maybe LBS.ByteString) m
      , HasReader "publicKey" Text m
      , HasReader "privateKey" BC.ByteString m
      , HasReader "manager" (Maybe Manager) m
      , HasReader "logContext" LogContext m
      , MonadIO m
      )
   => BitMEXRequest req contentType res accept
   -> m (MimeResult res)
makeRequest req@BitMEXRequest {..} = do
    pub <- ask @"publicKey"
    logCxt <- ask @"logContext"
    time <- liftIO $ makeTimestamp <$> getPOSIXTime
    config0 <-
        makeRESTConfig >>=
        liftIO . return . withLoggingBitMEXConfig logCxt
    let verb = filter (/= '"') $ show rMethod
    let query = rParams ^. paramsQueryL
    sig <-
        case rParams ^. paramsBodyL of
            ParamBodyBL lbs ->
                sign
                    (BC.pack
                         (verb <> "/api/v1" <>
                          (LBC.unpack . head) rUrlPath <>
                          BC.unpack (renderQuery True query) <>
                          show time <>
                          LBC.unpack lbs))
            ParamBodyB bs ->
                sign
                    (BC.pack
                         (verb <> "/api/v1" <>
                          (LBC.unpack . head) rUrlPath <>
                          BC.unpack (renderQuery True query) <>
                          show time <>
                          BC.unpack bs))
            _ ->
                sign
                    (BC.pack
                         (verb <> "/api/v1" <>
                          (LBC.unpack . head) rUrlPath <>
                          BC.unpack (renderQuery True query) <>
                          show time))
    let new =
            setHeader
                req
                [("api-expires", toByteString' time)]
        config =
            config0 `addAuthMethod`
            AuthApiKeyApiSignature ((T.pack . show) sig) `addAuthMethod`
            AuthApiKeyApiNonce "" `addAuthMethod`
            AuthApiKeyApiKey pub
    mgr <-
        ask @"manager" >>= \m ->
            case m of
                Nothing ->
                    liftIO $ newManager tlsManagerSettings
                Just x -> return x
    liftIO $ dispatchMime mgr config new

------------------------------------------------------------
-- WebSocket

-- | Establish connection to the BitMEX WebSocket API,
-- authenticate the user and subscribe to the provided topics.
withConnectAndSubscribe ::
       BitMEXWrapperConfig
    -> [Topic Symbol]
    -> ClientApp a
    -> IO a
withConnectAndSubscribe config@BitMEXWrapperConfig {..} ts app = do
    let base = (drop 8 . show) environment
        path =
            case pathWS of
                Nothing -> "/realtime"
                Just x  -> x
    withSocketsDo $
        runSecureClient base 443 (LBC.unpack path) $ \c -> do
            time <- makeTimestamp <$> getPOSIXTime
            sig <-
                    (run (sign
                              (BC.pack
                                   ("GET" ++
                                    "/realtime" ++ show time))) config)
            sendMessage
                c
                AuthKey
                [ String publicKey
                , toJSON time
                , (toJSON . show) sig
                ]
            sendMessage c Subscribe ts
            app c

-- | Establish connection to the BitMEX WebSocket API.
connect :: BitMEXWrapperConfig -> BitMEXApp () -> IO ()
connect initConfig@BitMEXWrapperConfig {..} app = do
    let base = (drop 8 . show) environment
        path =
            case pathWS of
                Nothing -> "/realtime"
                Just x  -> x
    config <-
        return $
        withLoggingBitMEXWrapper logContext initConfig
    withSocketsDo $
        runSecureClient base 443 (LBC.unpack path) $ \conn -> do
            run (app conn) config

-- | Receive a message from the WebSocket connection and parse it.
getMessage ::
       Connection
    -> BitMEXWrapperConfig
    -> IO (Maybe Response)
getMessage conn config = do
    msg <- receiveData conn
    runConfigLogWithExceptions "WebSocket" config $ do
        case (decode msg :: Maybe Response) of
            Nothing -> do
                errorLog msg
                return Nothing
            Just r -> do
                case r of
                    P _ -> do
                        log' "Positions" msg
                        return (Just r)
                    OB10 _ -> do
                        log' "OB10" msg
                        return (Just r)
                    Exe _ -> do
                        log' "Execution" msg
                        return (Just r)
                    O _ -> do
                        log' "Order" msg
                        return (Just r)
                    M _ -> do
                        log' "Margin" msg
                        return (Just r)
                    Error _ -> do
                        errorLog msg
                        return (Just r)
                    _ -> do
                        log' "WebSocket" msg
                        return (Just r)
  where
    log' s msg =
        _log s levelInfo $ (LT.toStrict . LT.decodeUtf8) msg
    errorLog msg =
        _log "WebSocket Error" levelError $
        (LT.toStrict . LT.decodeUtf8) msg

-- | Send a message to the WebSocket connection.
sendMessage ::
       (ToJSON a) => Connection -> Command -> [a] -> IO ()
sendMessage conn comm topics =
    sendTextData conn $
    encode $ Message {op = comm, args = fromList topics}
