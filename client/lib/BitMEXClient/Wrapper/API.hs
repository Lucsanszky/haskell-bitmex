module BitMEXClient.Wrapper.API
    ( makeRequest
    , connect
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
    , setHeader
    )
import           BitMEX.Logging
import           BitMEXClient.CustomPrelude
import           BitMEXClient.WebSockets.Types
    ( Command
    , Message (..)
    , Response (..)
    )
import           BitMEXClient.Wrapper.Logging
import           BitMEXClient.Wrapper.Types
import           Data.ByteArray
    ( ByteArrayAccess
    )
import qualified Data.ByteString.Char8         as BC
    ( pack
    , unpack
    )
import           Data.ByteString.Conversion
    ( toByteString'
    )
import qualified Data.ByteString.Lazy          as LBS
    ( append
    )
import qualified Data.ByteString.Lazy.Char8    as LBC
    ( pack
    , unpack
    )
import qualified Data.Text                     as T (pack)
import qualified Data.Text.Lazy                as LT
    ( toStrict
    )
import qualified Data.Text.Lazy.Encoding       as LT
    ( decodeUtf8
    )
import           Data.Vector                   (fromList)

sign ::
       (ByteArrayAccess a)
    => a
    -> BitMEXReader IO (Digest SHA256)
sign body = do
    secret <- asks privateKey
    return . hmacGetDigest . hmac secret $ body

makeRESTConfig :: BitMEXReader IO BitMEXConfig
makeRESTConfig = do
    env <- asks environment
    logCxt <- asks logContext
    path <-
        asks pathREST >>= \p ->
            return $
            case p of
                Nothing -> "/api/v1"
                Just x  -> x
    let base = (LBC.pack . show) env
        logExecContext = asks logExecContext
    return
        BitMEXConfig
        { configHost = LBS.append base path
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
    -> BitMEXReader IO (MimeResult res)
makeRequest req@BitMEXRequest {..} = do
    pub <- asks publicKey
    logCxtF <- asks logContextFunction
    time <- liftIO $ makeTimestamp <$> getPOSIXTime
    config0 <-
        makeRESTConfig >>= \c ->
            liftIO $
            (logCxtF (configLogContext c)) >>= \cxt ->
                withLoggingBitMEXConfig cxt c
    let verb = filter (/= '"') $ show rMethod
    sig <-
        case rParams ^. paramsBodyL of
            ParamBodyBL lbs ->
                sign
                    (BC.pack
                         (verb ++
                          "/api/v1" ++
                          (LBC.unpack . head) rUrlPath ++
                          show time ++ LBC.unpack lbs))
            ParamBodyB bs ->
                sign
                    (BC.pack
                         (verb ++
                          "/api/v1" ++
                          (LBC.unpack . head) rUrlPath ++
                          show time ++ BC.unpack bs))
            _ ->
                sign
                    (BC.pack
                         (verb ++
                          "/api/v1" ++
                          (LBC.unpack . head) rUrlPath ++
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
        asks manager >>= \m ->
            case m of
                Nothing ->
                    liftIO $ newManager tlsManagerSettings
                Just x -> return x
    liftIO $ dispatchMime mgr config new

connect :: BitMEXWrapperConfig -> BitMEXApp IO () -> IO ()
connect initConfig@BitMEXWrapperConfig {..} app = do
    let base = (drop 8 . show) environment
        path =
            case pathWS of
                Nothing -> "/realtime"
                Just x  -> x
    config <-
        logContextFunction (logContext) >>= \ctx ->
            withLoggingBitMEXWrapper ctx initConfig
    withSocketsDo $
        runSecureClient base 443 (LBC.unpack path) $ \conn -> do
            runReaderT (run (app conn)) config

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
                    Error _ -> do
                        errorLog msg
                        return (Just r)
                    _ -> do
                        fullLog msg
                        return (Just r)
  where
    fullLog msg =
        _log "WebSocket" levelInfo $
        (LT.toStrict . LT.decodeUtf8) msg
    errorLog msg =
        _log "WebSocket" levelError $
        (LT.toStrict . LT.decodeUtf8) msg

sendMessage ::
       (ToJSON a) => Connection -> Command -> [a] -> IO ()
sendMessage conn comm topics =
    sendTextData conn $
    encode $ Message {op = comm, args = fromList topics}
