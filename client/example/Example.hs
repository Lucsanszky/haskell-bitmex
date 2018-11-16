{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import qualified BitMEX                  as Mex
    ( Accept (..)
    , ContentType (..)
    , Leverage (..)
    , LogContext
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
    ( APIKeys (..)
    , Authenticator (..)
    , BitMEXApp
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
    , makeTimestamp
    , run
    , sendMessage
    , sign
    , withConnectAndSubscribe
    )
import           Capability.Reader
import           Control.Concurrent      (forkIO)
import           Control.Exception
import           Control.Monad           (forever, unless)
import           Control.Monad.Reader    (MonadIO, liftIO)
import qualified Control.Monad.Reader    as R (ask, asks)
import           Data.Aeson
    ( Value (String)
    , decode
    , toJSON
    )
import           Data.Aeson
import           Data.ByteString         (readFile)
import qualified Data.ByteString         as BS (ByteString)
import           Data.ByteString.Char8   (pack, takeWhile)
import qualified Data.ByteString.Lazy    as LBS (ByteString)
import           Data.Char               (isSpace)
import           Data.Monoid
import           Data.Text               (Text, null)
import qualified Data.Text               as T
    ( pack
    , stripEnd
    )
import qualified Data.Text.IO            as T
    ( getLine
    , readFile
    )
import           Data.Time.Clock.POSIX   (getPOSIXTime)
import           Katip                   hiding
    ( Environment
    )
import           Network.HTTP.Client
    ( Manager
    , newManager
    )
import           Network.HTTP.Client.TLS
    ( tlsManagerSettings
    )
import           Network.WebSockets
    ( Connection
    , receiveData
    , sendClose
    , sendTextData
    )
import           Prelude
    ( Bool (True)
    , IO
    , Maybe (..)
    , mempty
    , not
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
       ( Authenticator m
       , HasReader "environment" Environment m
       , HasReader "logContext" Mex.LogContext m
       , HasReader "manager" (Maybe Manager) m
       , HasReader "pathREST" (Maybe LBS.ByteString) m
       , MonadIO m
       )
    => Symbol
    -> Mex.Leverage
    -> m (Mex.MimeResult Mex.Position)
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

app :: ( Authenticator m
       , HasReader "config" BitMEXWrapperConfig m
       , HasReader "environment" Environment m
       , HasReader "logContext" Mex.LogContext m
       , HasReader "manager" (Maybe Manager) m
       , HasReader "pathREST" (Maybe LBS.ByteString) m
       , MonadIO m
       )
    => Connection
    -> m ()
app conn = do
    config <- ask @"config"
    time <- liftIO $ makeTimestamp <$> getPOSIXTime
    -- Example usage of makeRequest
    x <-
        makeRequest $
        Mex.orderGetOrders (Mex.Accept Mex.MimeJSON)
    _ <- updateLeverage XBTUSD (Mex.Leverage 3)
    liftIO $ do
        print x
        _ <-
            forkIO $
            run (authWSMessage time) config >>=
            sendMessage conn AuthKey
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
    let apiKeys =
            APIKeys
            { publicKey = T.stripEnd pub
            , privateKey = takeWhile (not . isSpace) priv
            }
    let config =
            BitMEXWrapperConfig
            { environment = TestNet
            , pathREST = Just "/api/v1"
            , pathWS = Just "/realtime"
            , manager = Just mgr
            , apiKeys = apiKeys
            , logContext = logCxt
            }
    -- Example usage of withConnectAndSubscribe
    withConnectAndSubscribe config [Margin] $ \c -> do
        getMessage c config >>= print
        sendClose c ("Connection closed" :: Text)
    -- Example usage of connect
    connect config app
