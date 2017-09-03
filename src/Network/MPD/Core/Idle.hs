{-# LANGUAGE LambdaCase #-}

module Network.MPD.Core.Idle (
    mpdIdle
) where

import           Network.MPD.Commands.Types (Subsystem)
import           Network.MPD.Commands.Status (idle, noidle)
import           Network.MPD.Core (MPD(..), MPDEnv(..), MPDError, IdleState(..))

import           Control.Concurrent.Async.Lifted (Async, async, wait, cancel)
import           Control.Concurrent.STM.TVar (TVar, readTVar, readTVarIO, writeTVar, newTVarIO)
import           Control.Exception.Safe (catchAny, throw, throwString)
import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Reader (asks)
import           Control.Monad.STM (atomically, retry)

mpdIdle :: [Subsystem] -> MPD (Async (Either MPDError [Subsystem]))
mpdIdle subsystems = do
    tIdleState <- MPD $ asks envIdleState
    idleAsync <- liftIO . atomically $ readTVar tIdleState >>= \case
      NotIdling -> writeTVar tIdleState Starting >> return Nothing
      Starting  -> retry
      Idling a  -> writeTVar tIdleState Starting >> return (Just a)
    case idleAsync of
      Just a ->  noidle
      Nothing -> return ()
    a <- async $ idle' tIdleState
    wasIdling <- liftIO . atomically $ readTVar tIdleState >>= \case
      Idling _ -> return True
      _        -> return False
    when wasIdling $ throwString "Absurd: idling thread should not be active"
    return a
    where
    idle' :: TVar IdleState -> MPD [Subsystem]
    idle' tIdleState = idle subsystems
      `catchAny` (\err -> do
          wasNotIdling <- liftIO . atomically $
            readTVar tIdleState >>= \case
                NotIdling -> return True
                _         -> writeTVar tIdleState NotIdling >> return False
          if wasNotIdling
            then throwString $
              "Absurd: error thrown from idling thread while state is "
              ++ "'NotIdling': " ++ show err
            else throw err
      )

mpdNoIdle :: MPD ()
mpdNoIdle = do
    tIdleState <- MPD $ asks envIdleState
    idleAsync <- liftIO . atomically $ readTVar tIdleState >>= \case
      NotIdling -> return Nothing
      Starting  -> retry
      Idling a  -> return $ Just a
    case idleAsync of
      Nothing -> return ()
      Just a -> cancel a >> liftIO . atomically $ writeTVar tIdleState NotIdling
