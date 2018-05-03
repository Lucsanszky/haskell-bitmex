{-# LANGUAGE OverloadedStrings #-}

module Example where

import           BitMEX
import           BitMEXWrapper.Type
import           BitMEXWrapper.Wrapper   (makeRequest)
import           Control.Monad.Reader    (runReaderT)
import           Data.ByteString         (readFile)
import qualified Data.Text.IO            as T (readFile)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS
    ( tlsManagerSettings
    )
import           Prelude
    ( IO
    , RealFrac
    , print
    , ($)
    )
import           System.Environment      (getArgs)

main :: IO ()
main = do
    mgr <- newManager tlsManagerSettings
    (pubPath : privPath : _) <- getArgs
    pub <- T.readFile pubPath
    priv <- readFile privPath
    let config =
            BitMEXWrapperConfig
            { url = "https://testnet.bitmex.com/api/v1"
            , manager = mgr
            , publicKey = pub
            , privateKey = priv
            }
    res <-
        runReaderT
            (run (makeRequest $
                  orderGetOrders (Accept MimeJSON)))
            config
    print res
