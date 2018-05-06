module BitMEXWrapper.Wrapper
    ( makeRequest
    , connect
    , sign
    , makeTimestamp
    ) where

import           BitMEX
import           BitMEXWrapper.Type
import           Control.Monad.Reader       (asks, liftIO)
import           Crypto.Hash                (Digest)
import           Crypto.Hash.Algorithms     (SHA256)
import           Crypto.MAC.HMAC
    ( hmac
    , hmacGetDigest
    )
import           Data.ByteArray
    ( ByteArrayAccess
    )
import           Data.ByteString.Char8      (pack)
import           Data.ByteString.Conversion (toByteString')
import           Data.ByteString.Lazy       (append)
import           Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.Text                  as T
    ( Text
    , pack
    , unpack
    )
import qualified Data.Text.IO               as T (readFile)
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Network.Socket             (withSocketsDo)
import           Network.WebSockets
    ( ClientApp
    , Connection
    )
import           Prelude
    ( Bool (..)
    , IO
    , Int
    , Maybe (..)
    , RealFrac
    , Show
    , filter
    , floor
    , head
    , print
    , return
    , show
    , ($)
    , (*)
    , (+)
    , (++)
    , (.)
    , (/=)
    , (>>=)
    )
import           System.Environment         (getArgs)
import           Wuss
    ( runSecureClient
    )

sign ::
       (ByteArrayAccess a, Show a)
    => a
    -> BitMEXReader (Digest SHA256)
sign body = do
    secret <- asks privateKey
    return . hmacGetDigest . hmac secret $ body

makeRESTConfig :: BitMEXReader BitMEXConfig
makeRESTConfig = do
    base <- asks url
    path <- asks path
    logCxt <- liftIO $ initLogContext
    return $
        BitMEXConfig
        { configHost = append base path
        , configUserAgent =
              "swagger-haskell-http-client/1.0.0"
        , configLogExecWithContext =
              runDefaultLogExecWithContext
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
    priv <- asks privateKey
    pub <- asks publicKey
    time <- liftIO $ getPOSIXTime >>= return . makeTimestamp
    config0 <- makeRESTConfig >>= liftIO . withStdoutLogging
    let verb = filter (/= '"') $ show rMethod
    sig <-
        sign
            (pack
                 (verb ++
                  "/api/v1" ++
                  (unpack . head) rUrlPath ++ show time))
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

connect ::
       ((Digest SHA256) -> Int -> T.Text -> Connection -> IO ())
    -> BitMEXReader ()
connect app = do
    base <- asks url
    path <- asks path
    priv <- asks privateKey
    pub <- asks publicKey
    time <- liftIO $ getPOSIXTime >>= return . makeTimestamp
    sig <- sign (pack ("GET" ++ "/realtime" ++ show time))
    liftIO . withSocketsDo $
        runSecureClient (unpack base) 443 (unpack path) $ \conn ->
            app sig time pub conn
