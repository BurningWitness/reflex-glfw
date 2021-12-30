{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Channel
  ( -- * Channel
    Channel
  , newBoundChannel
  , newUnboundChannel
    -- * Performing Events
  , PerformEventOn (..)
  , performEventAsyncOn
  ) where

import           Reflex.Host.GLFW.Internal (Channel (..), HostChannel (..))

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Bifunctor.Flip
import           Data.Dependent.Sum
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
  -> Event t Bool
  -> m (Channel, Event t Bool)
newChannel fork openCloseE = mdo

  (tchan, alive) <- liftIO . atomically $ (,) <$> newTQueue
                                              <*> newTMVar False

  let openE  = ffilter id openCloseE
      closeE = ffilter not openCloseE

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

                 for_ eventList $ \events ->
                   for_ events $ \(Flip outRef :=> io) ->
                     io >>= outRef

                 stillAlive <- atomically $ takeTMVar alive

                 if stillAlive
                   then do
                     atomically $ putTMVar alive True
                     loop
                   else do
                     callback False
                     atomically $ putTMVar alive False

              callback True
      ) <$ openE

  let channel = Channel tchan alive

  performEventOn_ channel $ atomically (() <$ swapTMVar alive False) <$ closeE

  return (channel, doneE)



-- | Initializes a new __bound__ 'Channel'.
--
--   The input 'Event' determines thread operation: passing @True@ creates a thread,
--   while passing @False@ makes the thread exit after it's done processing.
--
--   The output 'Event' is a mirror of the input one, confirming creation or exiting of
--   the thread. It is handled by the trigger thread and will properly
--   fire a @False@ if an exception is raised in the thread.
newBoundChannel
  :: ( MonadIO m
     , MonadIO (Performable m)
     , PerformEvent t m
     , ReflexHost t
     , TriggerEvent t m
     )
  => Event t Bool
  -> m (Channel, Event t Bool)
newBoundChannel = newChannel forkOS

-- | Initializes a new __unbound__ 'Channel'.
--
--   Same reasoning as with 'newBoundChannel'.
--
--   No real benefit over just using 'forkIO' on the trigger thread.
newUnboundChannel
  :: ( MonadIO m
     , MonadIO (Performable m)
     , PerformEvent t m
     , ReflexHost t
     , TriggerEvent t m
     )
  => Event t Bool
  -> m (Channel, Event t Bool)
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
