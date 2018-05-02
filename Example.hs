{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Example where

import           BitMEX
import           Crypto.Hash
import           Crypto.Hash.Algorithms
import           Crypto.MAC.HMAC
import           Data.ByteArray
    ( ByteArrayAccess
    )
import           Data.ByteString            (append)
import           Data.ByteString.Char8      (pack)
import           Data.ByteString.Conversion
import           Data.ByteString.Internal
import           Data.ByteString.Lazy       (toStrict)
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Hex
import qualified Data.Text                  as T
import           Data.Time.Clock.POSIX
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Numeric

-- TODO: Use a ReaderT to store these
secret = "api-private-key"

key = "api-public-key"

sign ::
       (ByteArrayAccess a, Show a)
    => a
    -> a
    -> IO (Digest SHA256)
sign secret body = do
    print body
    return . hmacGetDigest . hmac secret $ body

-- TODO: Factor out config initialization
testNetConfig :: IO BitMEXConfig
testNetConfig = do
    logCxt <- initLogContext
    return $
        BitMEXConfig
        { configHost = "https://testnet.bitmex.com/api/v1"
        , configUserAgent =
              "swagger-haskell-http-client/1.0.0"
        , configLogExecWithContext =
              runDefaultLogExecWithContext
        , configLogContext = logCxt
        , configAuthMethods = []
        , configValidateAuthMethods = True
        }

mainNetConfig :: IO BitMEXConfig
mainNetConfig = do
    logCxt <- initLogContext
    return $
        BitMEXConfig
        { configHost = "https://www.bitmex.com/api/v1"
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

-- TODO: Store the keys, the manager
-- and the config type (testnet or mainnet)
-- in a ReaderT
makeRequest ::
       ( Produces req accept
       , Show res
       , MimeUnrender accept res
       , MimeType contentType
       )
    => ByteString
    -> T.Text
    -> BitMEXRequest req contentType res accept
    -> IO ()
makeRequest secret pub req@BitMEXRequest {..} = do
    mgr <- newManager tlsManagerSettings
    time <- getPOSIXTime >>= return . makeTimestamp
    config0 <- testNetConfig >>= withStdoutLogging
    let verb = filter (/= '"') $ show rMethod
    sig <-
        sign
            secret
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
    res <- dispatchMime mgr config new
    print res

main :: IO ()
main = do
    makeRequest secret key $
        orderGetOrders (Accept MimeJSON)
