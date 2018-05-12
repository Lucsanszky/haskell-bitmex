module BitMEXWebSockets.API
    ( app
    ) where

import           BitMEXWebSockets.Types
import           Control.Concurrent     (forkIO)
import           Control.Monad          (forever, unless)
import           Control.Monad.Reader   (liftIO)
import           Crypto.Hash            (Digest)
import           Crypto.Hash.Algorithms (SHA256)
import           Data.Aeson
    ( Value (String)
    , decode
    , encode
    , toJSON
    )
import           Data.Text              (Text, null)
import           Data.Text.IO           (getLine)
import           Data.Vector            (fromList)
import           Network.WebSockets
    ( Connection
    , receiveData
    , sendClose
    , sendTextData
    )
import           Prelude
    ( IO
    , Int
    , Maybe
    , print
    , show
    , ($)
    , (.)
    , (>>)
    , (>>=)
    )

app :: (Digest SHA256) -> Int -> Text -> Connection -> IO ()
app sig time pub conn = do
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
        { op = AuthKey
        , args =
              fromList
                  [ String pub
                  , toJSON time
                  , (toJSON . show) sig
                  ]
        }
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
