module CustomLogging
(
LoggingRunner
) where

import Control.Concurrent
import qualified Control.Monad.Logger as LT

type LoggingRunner = LT.LoggingT IO ThreadId -> IO ThreadId
