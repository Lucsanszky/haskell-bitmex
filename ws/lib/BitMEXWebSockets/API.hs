module BitMEXWebSockets.API
    ( sendMessage
    ) where

import           BitMEXWebSockets.Types
import           Data.Aeson             (ToJSON, encode)
import           Data.Vector            (fromList)
import           Network.WebSockets
    ( Connection
    , sendTextData
    )
import           Prelude                (IO, ($))

sendMessage ::
       (ToJSON a) => Connection -> Command -> [a] -> IO ()
sendMessage conn comm topics =
    sendTextData conn $
    encode $ Message {op = comm, args = fromList topics}
