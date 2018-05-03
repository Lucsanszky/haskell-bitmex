{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module BitMEXWebSockets.API
    ( app
    ) where

import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Data.Text           (Text, null)
import           Data.Text.IO        (getLine, putStrLn)
import           Network.WebSockets
    ( ClientApp
    , receiveData
    , sendClose
    , sendTextData
    )
import           Prelude             (($), (>>), (>>=))

app :: ClientApp ()
app conn = do
    _ <-
        forkIO $
        forever $ do
            msg <- receiveData conn
            liftIO $ putStrLn msg
    loop
    sendClose conn ("Connection closed" :: Text)
  where
    loop =
        getLine >>= \line ->
            unless (null line) $
            sendTextData conn line >> loop
