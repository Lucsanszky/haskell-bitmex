module BitMEXClient.Wrapper.Util
    ( sign
    , makeTimestamp
    ) where

import           BitMEXClient.CustomPrelude

-- | Create a signature for the request.
sign ::
       (ByteArrayAccess secret, ByteArrayAccess message)
    => secret
    -> message
    -> Digest SHA256
sign s m = hmacGetDigest . hmac s $ m

-- | Convenience function to generate a timestamp
-- for the signature of the request.
-- FIX ME!!! THIS MUST BE FIXED at this constant for now.
makeTimestamp :: (RealFrac a) => a -> Int
makeTimestamp _ = 9999999999 -- floor . (* 1000000)
