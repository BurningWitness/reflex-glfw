{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , RecursiveDo
           , TypeFamilies
           , UndecidableInstances #-}

module Reflex.Channel
  ( -- * Channel
    Channel
  , newBoundChannel
  , newUnboundChannel
    -- * Performing Events
  , PerformEventOn (..)
  , performEventAsyncOn
  ) where

import           Reflex.Host.GLFW.Internal

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Bifunctor.Flip
import           Data.Dependent.Sum
import           Data.Dependent.Map as DMap
import           Data.Foldable (for_)
import           Graphics.UI.GLFW
import           Prelude
import           Reflex
import           Reflex.Host.Class



newChannel
  :: ( MonadIO m
     , MonadIO (Performable m)
     , PerformEvent t m
     , ReflexHost t
     , TriggerEvent t m
     )
  => (IO () -> IO ThreadId)
  -> EventSelector t (Action ())
  -> m (Channel, EventSelector t (Result () SomeException))
newChannel fork actionFan = mdo

  (tchan, alive) <- liftIO . atomically $ (,) <$> newTQueue
                                              <*> newTMVar False

  let openE  = select actionFan Create
      closeE = select actionFan Destroy

  doneE <-
    performEventAsync $
      ( \callback ->
          liftIO $ do
            isAlive <- atomically $ swapTMVar alive True

            unless isAlive $ do
              void . fork $
                fix $ \loop -> do

                 eventList <- atomically $ do
                                el <- readTQueue tchan
                                elRest <- fix $ \loop2 -> do
                                            mayRes <- tryReadTQueue tchan
                                            case mayRes of
                                              Just res -> (:) res <$> loop2
                                              Nothing  -> return []
                                return $ el : elRest

                 handle (\e -> do callback $ DMap.singleton Failed (pure e)
                                  _ <- atomically $ swapTMVar alive False
                                  throw e
                        ) $
                   for_ eventList $ \events ->
                     for_ events $ \(Flip outRef :=> io) ->
                       io >>= outRef

                 stillAlive <- atomically $ takeTMVar alive

                 if stillAlive
                   then do
                     atomically $ putTMVar alive True
                     loop
                   else do
                     callback $ DMap.singleton Destroyed (pure ())
                     atomically $ putTMVar alive False

              callback $ DMap.singleton Created (pure ())
      ) <$ openE

  let channel = Channel tchan alive

  performEventOn_ channel $ atomically (() <$ swapTMVar alive False) <$ closeE

  return (channel, fan doneE)



-- | Initializes a new 'Channel' on a __bound__ thread.
--
--   Any exception raised on this channel returns a 'Failed' result holding said exception,
--   then rethrows it to crash the thread the channel runs on.
newBoundChannel
  :: ( MonadIO m
     , MonadIO (Performable m)
     , PerformEvent t m
     , ReflexHost t
     , TriggerEvent t m
     )
  => EventSelector t (Action ())
  -> m (Channel, EventSelector t (Result () SomeException))
newBoundChannel = newChannel forkOS

-- | Initializes a new 'Channel' channel on an __unbound__ thread.
--
--   Same reasoning as with 'newBoundChannel'.
--
--   No real benefit over just using 'forkIO' on the trigger thread other than perhaps
--   exception handling.
newUnboundChannel
  :: ( MonadIO m
     , MonadIO (Performable m)
     , PerformEvent t m
     , ReflexHost t
     , TriggerEvent t m
     )
  => EventSelector t (Action ())
  -> m (Channel, EventSelector t (Result () SomeException))
newUnboundChannel = newChannel forkIO



-- | Similar to 'PerformEvent', relaying the 'Event' and executing it on a 'Channel'.
class ( Monad m
      , MonadIO (Performable m)
      , Reflex t
      )
     => PerformEventOn f t m | m -> t where
  -- | The return 'Event' is fired by the trigger thread.
  performEventOn :: f -> Event t (IO a) -> m (Event t a)

  performEventOn_ :: f -> Event t (IO ()) -> m ()

instance ( MonadIO (Performable m)
         , PerformEvent t m
         , TriggerEvent t m
         )
        => PerformEventOn Channel t m where
  performEventOn (Channel tchan alive) ioE = do
    (outE, outRef) <- newTriggerEvent

    performEvent_ $ ( \io -> liftIO .
                               atomically $ do
                                 isAlive <- readTMVar alive
                                 when isAlive $
                                   writeTQueue tchan [Flip outRef :=> io]
                    ) <$> ioE

    return outE

  performEventOn_ (Channel tchan alive) ioE =
    performEvent_ $ ( \io -> liftIO .
                               atomically $ do
                                 isAlive <- readTMVar alive
                                 when isAlive $
                                   writeTQueue tchan [Flip (const $ return ()) :=> io]
                    ) <$> ioE

instance ( MonadIO (Performable m)
         , PerformEvent t m
         , TriggerEvent t m
         )
        => PerformEventOn HostChannel t m where
  performEventOn (HostChannel tchan) ioE = do
    (outE, outRef) <- newTriggerEvent

    performEvent_ $ ( \io -> liftIO $ do
                               atomically $
                                 writeTQueue tchan [Flip outRef :=> io]
                               postEmptyEvent
                    ) <$> ioE

    return outE

  performEventOn_ (HostChannel tchan) ioE =
    performEvent_ $ ( \io -> liftIO $ do
                               atomically $
                                 writeTQueue tchan [Flip (const $ return ()) :=> io]
                               postEmptyEvent
                    ) <$> ioE




-- | Similar to 'performEventAsyncOn', relaying the 'Event' after the callback gets
--   called and executing it on a 'Channel'.
--
--   The return 'Event' is fired by the trigger thread.
performEventAsyncOn
  :: ( PerformEventOn f t m
     , TriggerEvent t m
     )
  => f
  -> Event t ((a -> IO ()) -> IO ())
  -> m (Event t a)
performEventAsyncOn chan actionE = do
  (outE, outRef) <- newTriggerEvent

  performEventOn_ chan $ fmap ($ outRef) actionE

  return outE
