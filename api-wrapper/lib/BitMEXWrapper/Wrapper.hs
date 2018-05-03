module BitMEXWrapper.Wrapper
    ( makeRequest
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
import           Data.ByteString            (append)
import           Data.ByteString.Char8      (pack)
import           Data.ByteString.Conversion (toByteString')
import           Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.Text                  as T (pack)
import qualified Data.Text.IO               as T (readFile)
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Prelude
    ( Bool (..)
    , IO
    , Int
    , RealFrac
    , Show
    , filter
    , floor
    , head
    , return
    , show
    , ($)
    , (+)
    , (++)
    , (.)
    , (/=)
    , (>>=)
    )
import           System.Environment         (getArgs)

sign ::
       (ByteArrayAccess a, Show a)
    => a
    -> BitMEXReader (Digest SHA256)
sign body = do
    secret <- asks privateKey
    return . hmacGetDigest . hmac secret $ body

makeRESTConfig :: BitMEXReader BitMEXConfig
makeRESTConfig = do
    host <- asks url
    logCxt <- liftIO $ initLogContext
    return $
        BitMEXConfig
        { configHost = host
        , configUserAgent =
              "swagger-haskell-http-client/1.0.0"
        , configLogExecWithContext =
              runDefaultLogExecWithContext
        , configLogContext = logCxt
        , configAuthMethods = []
        , configValidateAuthMethods = True
        }

makeTimestamp :: (RealFrac a) => a -> Int
makeTimestamp = (+ 5) . floor

makeRequest ::
       ( Produces req accept
       , MimeUnrender accept res
       , MimeType contentType
       )
    => BitMEXRequest req contentType res accept
    -> BitMEXReader (MimeResult res)
makeRequest req@BitMEXRequest {..} = do
    mgr <- asks manager
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
-- main :: IO ()
-- main = do
--     mgr <- newManager tlsManagerSettings
--     (pubPath : privPath : _) <- getArgs
--     pub <- T.readFile pubPath
--     priv <- readFile privPath
--     let config =
--             BitMEXWrapperConfig
--             { url = "https://testnet.bitmex.com/api/v1"
--             , manager = mgr
--             , publicKey = pub
--             , privateKey = priv
--             }
--     res <-
--         runReaderT
--             (run (makeRequest $
--                   orderGetOrders (Accept MimeJSON)))
--             config
--     print res
