module BitMEXWebSockets.API
    ( sendMessage
    ) where

import           BitMEXWebSockets.Types
import           Data.Aeson             (encode)
import           Data.Vector            (fromList)
import           Network.WebSockets
    ( Connection
    , sendTextData
    )
import           Prelude                (IO, ($))

sendMessage ::
       Connection -> Command -> [Topic Symbol] -> IO ()
sendMessage conn comm topics =
    sendTextData conn $
    encode $ Message {op = comm, args = fromList topics}
