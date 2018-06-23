module BitMEXClient.Wrapper.Logging
    ( withLoggingBitMEXWrapper
    , withLoggingBitMEXConfig
    , runConfigLog
    , runConfigLogWithExceptions
    ) where

import           BitMEX.Core
    ( BitMEXConfig (..)
    )
import           BitMEX.Logging
import           BitMEXClient.CustomPrelude
import           BitMEXClient.Wrapper.Types
import           Data.Text                  (Text)

withLoggingBitMEXWrapper :: LogContext -> BitMEXWrapperConfig -> IO BitMEXWrapperConfig
withLoggingBitMEXWrapper context config = return $
    config
    { logExecContext = runDefaultLogExecWithContext
    , logContext = context
    }

withLoggingBitMEXConfig :: LogContext -> BitMEXConfig -> IO BitMEXConfig
withLoggingBitMEXConfig context config = return $
    config
    { configLogExecWithContext = runDefaultLogExecWithContext
    , configLogContext = context
    }

runConfigLog ::
       MonadIO m => BitMEXWrapperConfig -> LogExec m
runConfigLog config =
    logExecContext config (logContext config)

runConfigLogWithExceptions ::
       (MonadCatch m, MonadIO m)
    => Text
    -> BitMEXWrapperConfig
    -> LogExec m
runConfigLogWithExceptions src config =
    runConfigLog config . logExceptions src
