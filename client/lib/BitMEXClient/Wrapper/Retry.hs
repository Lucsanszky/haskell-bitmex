{-# LANGUAGE ScopedTypeVariables #-}

module BitMEXClient.Wrapper.Retry
    ( Delay
    , RetryAction(..)
    , RetryPolicy
    , retry
    , retryXTimesWithDelayPolicy
    , exponential429BackOff
    , ThreadSleep
    , threadSleep
    ) where

import Prelude
import Data.Either              (Either(..), lefts)
import Data.Time                (UTCTime)

import Control.Monad.Time       (MonadTime, currentTime)
import Control.Concurrent       (threadDelay)

import Network.HTTP.Client      (Response(..))
import Network.HTTP.Types       (Status(..), serviceUnavailable503, badGateway502, tooManyRequests429)

import BitMEX                   (MimeResult(..), MimeError(..))

----------------------------------------
class ThreadSleep m where
    threadSleep :: Int -> m ()

instance ThreadSleep IO where
    threadSleep = threadDelay

----------------------------------------
-- | This should really only be a Natural number (in microseconds), rather than an Int
type Delay = Int

data RetryAction res = ReturnResult res | RetryAfter Delay

-- | Given the currentTime and a (possibly empty) list of what happened on previous attempts, tells us what to do next.
type RetryPolicy res = UTCTime -> [res] -> RetryAction res

----------------------------------------
retry :: forall m res. (MonadTime m, ThreadSleep m) => RetryPolicy res -> m res -> m res
retry policy action = do
    now <- currentTime
    retry' now []
  where
    retry' :: UTCTime -> [res] -> m res
    retry' now previousResults
        | ReturnResult res <- policy now previousResults = pure res
        | RetryAfter delay <- policy now previousResults = do
                                                            threadSleep delay
                                                            res  <- action -- may take a while
                                                            now' <- currentTime
                                                            retry' now' (res : previousResults)
        | otherwise = error "retry' - RetryAction should only have 2 constructors, this should be impossible!"

----------------------------------------
-- This is an example policy.
-- It retries up to X times (for a maximum of X+1 attempts in total)
-- while receiving errors in the designated list (e.g. [503,502,429]).
-- It uses the specified delay between attempts.
retryXTimesWithDelayPolicy :: [Int] -> Int -> Delay -> RetryPolicy (MimeResult res)
retryXTimesWithDelayPolicy          _     _         _ _      [] = RetryAfter 0  -- never attempted, try immediately
retryXTimesWithDelayPolicy errorCodes count uSecDelay _ results =
    let lastResult = head results
     in case lastResult of
            -- success
            MimeResult {mimeResult = Right _} -> ReturnResult lastResult
            -- retry-able failure
            MimeResult {mimeResult = Left MimeError {mimeErrorResponse = response}}
                | Status{statusCode = errorCode} <- responseStatus response
                , errorCode `elem` errorCodes -> if length results <= count
                                                    then RetryAfter   uSecDelay
                                                    else ReturnResult lastResult
            -- fatal failure
            _ -> ReturnResult lastResult


----------------------------------------
-- This policy does an exponential back-off (2x) every time it hits a 429.
-- The delay increases maxes out at 32 seconds. If another 429 is received
-- after that, we give up. This may be too lenient, it might be safer to
-- force an error if waiting for 16 seconds did not solve the problem.
--
-- If it gets a 503 or 502, it retries (immediately for 503). Those errors are
-- not our own fault, but they keep the execution thread tied up. If execution
-- is single threaded and is busy retrying, no other orders can execute and
-- may just be queued up. Adding unnecessary retry delays increases the chance
-- that once queued up orders are placed, their prices will be outdated.
--
-- This gives up after:
--  - 20 errors of type 502
--  - 30 errors of type 503 (load shedding)
--  -  6 errors of type 429 (rate limiting)
--
-- Because some retries are immediate, we may actually hit those high error counts.
--
exponential429BackOff :: RetryPolicy (MimeResult res)
exponential429BackOff _ []      = RetryAfter 0  -- never attempted, try immediately
exponential429BackOff _ results = case lastResult of
    -- success
    MimeResult {mimeResult = Right _} -> ReturnResult lastResult
    -- retry-able failure
    MimeResult {mimeResult = Left (MimeError {mimeErrorResponse = response})}
        | responseStatus response == badGateway502         && errorCount badGateway502         < 20 -> RetryAfter 100000
        | responseStatus response == serviceUnavailable503 && errorCount serviceUnavailable503 < 30 -> RetryAfter 0
        | responseStatus response == tooManyRequests429    && errorCount tooManyRequests429    <  6 -> RetryAfter (2 ^ errorCount tooManyRequests429 * 1000000)
    -- fatal failure
    _ -> ReturnResult lastResult

  where
    lastResult = head results

    errorCount :: Status -> Int
    errorCount errorType = length $ filter (== errorType) $ fmap (responseStatus . mimeErrorResponse) $ lefts $ fmap mimeResult $ results
