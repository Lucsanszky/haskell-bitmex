module BitMEXWebSockets.API
    ( app
    ) where

import           BitMEXWebSockets.Type
import           BitMEXWrapper
import           Control.Concurrent     (forkIO)
import           Control.Monad          (forever, unless)
import           Control.Monad.Reader   (asks, liftIO)
import           Crypto.Hash            (Digest)
import           Crypto.Hash.Algorithms (SHA256)
import           Data.Aeson             (encode)
import           Data.ByteString.Char8  (pack)
import qualified Data.Text              as T
    ( Text
    , null
    , pack
    )
import           Data.Text.IO           (getLine, putStrLn)
import           Data.Time.Clock.POSIX  (getPOSIXTime)
import           Network.WebSockets
    ( ClientApp
    , Connection
    , receiveData
    , sendClose
    , sendTextData
    )
import           Prelude
    ( IO
    , Int
    , filter
    , print
    , return
    , show
    , ($)
    , (++)
    , (.)
    , (/=)
    , (>>)
    , (>>=)
    )

signWS :: Connection -> BitMEXReader ()
signWS conn = do
    priv <- asks privateKey
    pub <- asks publicKey
    time <- liftIO $ getPOSIXTime >>= return . makeTimestamp
    sig <- sign (pack ("GET/realtime" ++ show time))
    liftIO $ print sig
    liftIO $
        sendTextData conn $
        encode $
        Message {op = Subscribe, args = [Connected]}
        -- Message
        -- { op = AuthKey
        -- , args = [pub, (T.pack . show) time, (T.pack . show) sig]
        -- }
    liftIO $ loop
    liftIO $ sendClose conn ("Connection closed" :: T.Text)
  where
    loop =
        getLine >>= \line ->
            unless (T.null line) $
            sendTextData conn line >> loop

app :: (Digest SHA256)
    -> Int
    -> T.Text
    -> Connection
    -> IO ()
app sig time pub conn = do
    _ <-
        forkIO $
        forever $ do
            msg <- receiveData conn
            liftIO $ putStrLn msg
    sendTextData conn $
        T.pack
            ("{\"op\": \"authKey\", \"args\": [" ++
             (show pub) ++
             "," ++
             (show time) ++
             "," ++ "\"" ++ (show sig) ++ "\"" ++ "]}")
    sendTextData conn $
        encode $ Message {op = Subscribe, args = [Chat]}
    loop
    sendClose conn ("Connection closed" :: T.Text)
  where
    loop =
        getLine >>= \line ->
            unless (T.null line) $
            sendTextData conn line >> loop
