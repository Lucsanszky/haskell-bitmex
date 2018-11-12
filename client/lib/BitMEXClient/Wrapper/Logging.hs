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

-- | Add a logging environment and an executor to a BitMEXWrapperConfig.
withLoggingBitMEXWrapper :: LogContext -> BitMEXWrapperConfig -> BitMEXWrapperConfig
withLoggingBitMEXWrapper context config =
    config
    { logContext = context
    }

-- | Add a logging environment and an executor to a BitMEXConfig.
withLoggingBitMEXConfig :: LogContext -> BitMEXConfig -> BitMEXConfig
withLoggingBitMEXConfig context config =
    config
    { configLogContext = context
    }

-- | Run the BitMEXWrapperConfig's executor on the config's
-- log environment.
runConfigLog ::
       MonadIO m => BitMEXWrapperConfig -> LogExec m
runConfigLog config =
    runDefaultLogExecWithContext (logContext config)

-- | Run the BitMEXWrapperConfig's executor on the config's
-- log environment with exceptions.
runConfigLogWithExceptions ::
       (MonadCatch m, MonadIO m)
    => Text
    -> BitMEXWrapperConfig
    -> LogExec m
runConfigLogWithExceptions src config =
    runConfigLog config . logExceptions src
