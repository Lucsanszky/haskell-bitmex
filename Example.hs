{-# LANGUAGE OverloadedStrings #-}

module Example where

import           BitMEX
import qualified BitMEXWebSockets        as WS
import           BitMEXWrapper
import           Control.Monad.Reader    (runReaderT)
import           Data.ByteString         (readFile)
import           Data.ByteString         (ByteString)
import           Data.Text               (Text)
import qualified Data.Text.IO            as T (readFile)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS
    ( tlsManagerSettings
    )
import           Prelude
    ( IO
    , Maybe (..)
    , RealFrac
    , print
    , ($)
    )
import           System.Environment      (getArgs)

ws :: IO ()
ws = do
    let pub = ""
        priv = ""
        config =
            BitMEXWrapperConfig
            { url = "testnet.bitmex.com"
            , path = "/realtime"
            , manager = Nothing
            , publicKey = pub
            , privateKey = priv
            }
    runReaderT (run (connect WS.app)) config

main :: IO ()
main = do
    mgr <- newManager tlsManagerSettings
    (pubPath:privPath:_) <- getArgs
    pub <- T.readFile pubPath
    priv <- readFile privPath
    let config =
            BitMEXWrapperConfig
            { url = "https://testnet.bitmex.com"
            , path = "/api/v1"
            , manager = Just mgr
            , publicKey = pub
            , privateKey = priv
            }
    res <-
        runReaderT
            (run (makeRequest $
                  orderGetOrders (Accept MimeJSON)))
            config
    print res
