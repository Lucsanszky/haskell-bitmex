module BitMEXWebSockets.API
    ( app
    ) where

import           BitMEXWebSockets.Types
import           Control.Concurrent     (forkIO)
import           Control.Monad          (forever, unless)
import           Control.Monad.Reader   (liftIO)
import           Data.Aeson             (decode, encode)
import           Data.Text              (Text, null)
import           Data.Text.IO           (getLine)
import           Data.Vector            (fromList)
import           Network.WebSockets
    ( ClientApp
    , receiveData
    , sendClose
    , sendTextData
    )
import           Prelude
    ( Maybe
    , print
    , show
    , ($)
    , (.)
    , (>>)
    , (>>=)
    )

app :: ClientApp ()
app conn = do
    _ <-
        forkIO $
        forever $ do
            msg <- receiveData conn
            liftIO $
                (print . show)
                    (decode msg :: Maybe Response)
    forkIO $
        sendTextData conn $
        encode $
        Message
        { op = Subscribe
        , args =
              fromList [OrderBook10 XBTUSD :: Topic Symbol]
        }
    loop
    sendClose conn ("Connection closed" :: Text)
  where
    loop =
        getLine >>= \line ->
            unless (null line) $
            sendTextData conn line >> loop
