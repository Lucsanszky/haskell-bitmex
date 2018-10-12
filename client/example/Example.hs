{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified BitMEX                  as Mex
    ( Accept (..)
    , ContentType (..)
    , Leverage (..)
    , MimeJSON (..)
    , MimeResult
    , Position
    , Symbol (..)
    , initLogContext
    , orderGetOrders
    , positionUpdateLeverage
    , runDefaultLogExecWithContext
    , stdoutLoggingContext
    , _setBodyLBS
    )
import           BitMEXClient
    ( BitMEXApp
    , BitMEXReader
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
    , withConnectAndSubscribe
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
import           Data.Aeson
import           Data.ByteString         (readFile)
import           Data.ByteString.Char8   (pack)
import           Data.Monoid
import           Data.Text               (Text, null)
import qualified Data.Text               as T (pack)
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

updateLeverage ::
       Symbol
    -> Mex.Leverage
    -> BitMEXReader (Mex.MimeResult Mex.Position)
updateLeverage sym lev = do
    let leverageTemplate =
            Mex.positionUpdateLeverage
                (Mex.ContentType Mex.MimeJSON)
                (Mex.Accept Mex.MimeJSON)
                (Mex.Symbol ((T.pack . show) sym))
                lev
        leverageRequest =
            Mex._setBodyLBS leverageTemplate $
            "{\"leverage\": " <> encode (Mex.unLeverage lev) <>
            ", \"symbol\": " <>
            encode ((T.pack . show) sym) <>
            "}"
    makeRequest leverageRequest

app :: BitMEXApp ()
app conn = do
    config <- R.ask
    pub <- R.asks publicKey
    time <- liftIO $ makeTimestamp <$> getPOSIXTime
    sig <- sign (pack ("GET" <> "/realtime" <> show time))
    -- Example usage of makeRequest
    x <-
        makeRequest $
        Mex.orderGetOrders (Mex.Accept Mex.MimeJSON)
    _ <- updateLeverage XBTUSD (Mex.Leverage 3)
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
        _ <-
            forkIO $
            forever $ do getMessage conn config >>= print
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
    logCxt <- Mex.initLogContext
    let config =
            BitMEXWrapperConfig
            { environment = TestNet
            , pathREST = Just "/api/v1"
            , pathWS = Just "/realtime"
            , manager = Just mgr
            , publicKey = pub
            , privateKey = priv
            , logExecContext =
                  Mex.runDefaultLogExecWithContext
            , logContext = logCxt
            }
    -- Example usage of withConnectAndSubscribe
    withConnectAndSubscribe config [Margin] $ \c -> do
        getMessage c config >>= print
        sendClose c ("Connection closed" :: Text)
    -- Example usage of connect
    connect config app
