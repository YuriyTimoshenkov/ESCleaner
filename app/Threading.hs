module Threading
(
  AsyncHandler
, AsyncResult
, MVarAsync
, run
, Threading.wait
) where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception.Base
import           Control.Concurrent.Async


data AsyncResult a = AsyncResult a

class AsyncHandler b where
  run :: (Show a) => IO a -> IO (AsyncResult (b a))
  wait :: (Show a) => AsyncResult (b a) -> IO (Either SomeException a)

newtype MVarAsync a = MVarAsync (MVar (Either SomeException  a))

instance AsyncHandler MVarAsync where
  run io =
    let onEnd mvar threadResult = putMVar mvar threadResult
    in do
      mvar <- newEmptyMVar
      threadId <- forkFinally io (onEnd mvar)
      return $ AsyncResult $ MVarAsync mvar

  wait (AsyncResult (MVarAsync aResult)) = takeMVar aResult


instance AsyncHandler Async where
  run io = do
    ar <- async io
    return $ AsyncResult ar

  wait (AsyncResult aResult) = waitCatch aResult
