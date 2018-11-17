{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import qualified BitMEX                  as Mex
import           BitMEXClient
import           Capability.Reader       (HasReader, ask)
import           Control.Concurrent      (forkIO)
import           Control.Monad           (forever, unless)
import           Control.Monad.Reader    (MonadIO, liftIO)
import           Data.Aeson              (encode)
import qualified Data.ByteString         as BS (readFile)
import qualified Data.ByteString.Char8   as BC (takeWhile)
import qualified Data.ByteString.Lazy    as LBS (ByteString)
import           Data.Char               (isSpace)
import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import qualified Data.Text               as T
    ( null
    , pack
    , stripEnd
    )
import qualified Data.Text.IO            as T
    ( getLine
    , readFile
    )
import           Data.Time.Clock.POSIX   (getPOSIXTime)
import           Network.HTTP.Client
    ( Manager
    , newManager
    )
import           Network.HTTP.Client.TLS
    ( tlsManagerSettings
    )
import           Network.WebSockets
    ( Connection
    , sendClose
    , sendTextData
    )
import           System.Environment      (getArgs)

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
            unless (T.null line) $
            sendTextData conn line >> loop

main :: IO ()
main = do
    mgr <- newManager tlsManagerSettings
    (pubPath:privPath:_) <- getArgs
    pub <- T.readFile pubPath
    priv <- BS.readFile privPath
    logCxt <- Mex.initLogContext
    let keys =
            APIKeys
            { publicKey = T.stripEnd pub
            , privateKey = BC.takeWhile (not . isSpace) priv
            }
    let config =
            BitMEXWrapperConfig
            { environment = TestNet
            , pathREST = Just "/api/v1"
            , pathWS = Just "/realtime"
            , manager = Just mgr
            , apiKeys = keys
            , logContext = logCxt
            }
    -- Example usage of withConnectAndSubscribe
    withConnectAndSubscribe config [Margin] $ \c -> do
        getMessage c config >>= print
        sendClose c ("Connection closed" :: Text)
    -- Example usage of connect
    connect config app
