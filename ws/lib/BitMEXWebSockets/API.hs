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
import           Data.Aeson
    ( Value
    , decode
    , encode
    )
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
    , Maybe
    , String
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
            liftIO $ (print . show) (decode msg :: Maybe Response)
    forkIO $ sendTextData conn $
        T.pack
            ("{\"op\": \"authKey\", \"args\": [" ++
             (show pub) ++
             "," ++
             (show time) ++
             "," ++ "\"" ++ (show sig) ++ "\"" ++ "]}")
    forkIO $ sendTextData conn $
        encode $ Message {op = Subscribe, args = [OrderBookL2 XBTUSD :: Topic Symbol]}
    forkIO $ sendTextData conn $
        encode $ Message {op = Subscribe, args = [OrderBookL2 XBTM18 :: Topic Symbol]}
    loop
    sendClose conn ("Connection closed" :: T.Text)
  where
    loop =
        getLine >>= \line ->
            unless (T.null line) $
            sendTextData conn line >> loop
