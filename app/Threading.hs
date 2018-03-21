module Threading
(
  forkIOWithAwaiter
) where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception.Base


forkIOWithAwaiter :: (Show a) => IO a -> IO (MVar (Either SomeException a))
forkIOWithAwaiter io =
  let onEnd mvar threadResult = putMVar mvar threadResult
  in do
    mvar <- newEmptyMVar
    threadId <- forkFinally io (onEnd mvar)
    return mvar
