{-# LANGUAGE OverloadedStrings #-}

module Example where

import           BitMEX
import           BitMEXWebSockets
import           BitMEXWrapper
import           Control.Concurrent      (forkIO)
import           Control.Monad           (forever, unless)
import           Control.Monad.Reader    (liftIO)
import           Control.Monad.Reader    (runReaderT)
import           Data.Aeson              (decode)
import           Data.ByteString         (readFile)
import           Data.Text               (Text, null)
import qualified Data.Text.IO            as T
    ( getLine
    , readFile
    )
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS
    ( tlsManagerSettings
    )
import           Network.WebSockets
    ( ClientApp
    , receiveData
    , sendClose
    , sendTextData
    )
import           Prelude
    ( IO
    , Maybe (..)
    , print
    , show
    , ($)
    , (.)
    , (>>)
    , (>>=)
    )
import           System.Environment      (getArgs)

app :: ClientApp ()
app conn = do
    _ <-
        forkIO $
        forever $ do
            msg <- receiveData conn
            liftIO $
                (print . show)
                    (decode msg :: Maybe Response)
    forkIO $ sendMessage conn Subscribe [OrderBook10 XBTUSD]
    loop
    sendClose conn ("Connection closed" :: Text)
  where
    loop =
        T.getLine >>= \line ->
            unless (null line) $
            sendTextData conn line >> loop

main :: IO ()
main = do
    mgr <- newManager tlsManagerSettings
    (pubPath:privPath:_) <- getArgs
    pub <- T.readFile pubPath
    priv <- readFile privPath
    let configREST =
            BitMEXWrapperConfig
            { url = "https://testnet.bitmex.com"
            , path = "/api/v1"
            , manager = Just mgr
            , publicKey = pub
            , privateKey = priv
            }
        configWS =
            BitMEXWrapperConfig
            { url = "testnet.bitmex.com"
            , path = "/realtime"
            , manager = Nothing
            , publicKey = pub
            , privateKey = priv
            }
    res <-
        runReaderT
            (run (makeRequest $
                  orderGetOrders (Accept MimeJSON)))
            configREST
    print res
    runReaderT (run (connect app)) configWS
