module BitMEXClient.WebSockets.API
    ( sendMessage
    ) where

import           BitMEXClient.CustomPrelude
import           BitMEXClient.WebSockets.Types
import           Data.Vector                   (fromList)

sendMessage ::
       (ToJSON a) => Connection -> Command -> [a] -> IO ()
sendMessage conn comm topics =
    sendTextData conn $
    encode $ Message {op = comm, args = fromList topics}
