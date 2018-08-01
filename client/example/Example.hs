{-# LANGUAGE OverloadedStrings #-}

module Main where

import           BitMEX
    ( Accept (..)
    , MimeJSON (..)
    , initLogContext
    , orderGetOrders
    , runDefaultLogExecWithContext
    , stdoutLoggingContext
    )
import           BitMEXClient
    ( BitMEXApp
    , BitMEXWrapperConfig (..)
    , Command (..)
    , Environment (..)
    , Symbol (..)
    , Topic (..)
    , connect
    , getMessage
    , makeRequest
    , makeTimestamp
    , sendMessage
    , sign
    )
import           Control.Concurrent      (forkIO)
import           Control.Exception
import           Control.Monad           (forever, unless)
import           Control.Monad.Reader    (liftIO)
import qualified Control.Monad.Reader    as R (ask, asks)
import           Data.Aeson
    ( Value (String)
    , decode
    , toJSON
    )
import           Data.ByteString         (readFile)
import           Data.ByteString.Char8   (pack)
import           Data.Text               (Text, null)
import qualified Data.Text.IO            as T
    ( getLine
    , readFile
    )
import           Data.Time.Clock.POSIX   (getPOSIXTime)
import           Katip
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS
    ( tlsManagerSettings
    )
import           Network.WebSockets
    ( receiveData
    , sendClose
    , sendTextData
    )
import           Prelude
    ( Bool (True)
    , IO
    , Maybe (..)
    , mempty
    , print
    , return
    , show
    , ($)
    , (++)
    , (.)
    , (<$>)
    , (=<<)
    , (>>)
    , (>>=)
    )
import           System.Environment      (getArgs)
import           System.IO               (stdout)

app :: BitMEXApp ()
app conn = do
    config <- R.ask
    pub <- R.asks publicKey
    time <- liftIO $ makeTimestamp <$> getPOSIXTime
    sig <- sign (pack ("GET" ++ "/realtime" ++ show time))
    x <- makeRequest $ orderGetOrders (Accept MimeJSON)
    liftIO $ do
        print x
        _ <-
            forkIO $
            sendMessage
                conn
                AuthKey
                [ String pub
                , toJSON time
                , (toJSON . show) sig
                ]
        _ <- forkIO $ forever $ do getMessage conn config
        _ <-
            forkIO $
            sendMessage
                conn
                Subscribe
                [OrderBook10 XBTUSD :: Topic Symbol]
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
    logCxt <- initLogContext
    let config =
            BitMEXWrapperConfig
            { environment = TestNet
            , pathREST = Just "/api/v1"
            , pathWS = Just "/realtime"
            , manager = Just mgr
            , publicKey = pub
            , privateKey = priv
            , logExecContext = runDefaultLogExecWithContext
            , logContext = logCxt
            }
    connect config app
