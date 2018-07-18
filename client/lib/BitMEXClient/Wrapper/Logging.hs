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

withLoggingBitMEXWrapper :: LogContext -> BitMEXWrapperConfig -> BitMEXWrapperConfig
withLoggingBitMEXWrapper context config =
    config
    { logExecContext = runDefaultLogExecWithContext
    , logContext = context
    }

withLoggingBitMEXConfig :: LogContext -> BitMEXConfig -> BitMEXConfig
withLoggingBitMEXConfig context config =
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
