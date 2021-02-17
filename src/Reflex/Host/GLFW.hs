{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Host.GLFW
  ( -- * Host
    GlobalE (..)
  , HostChannel
  , module Reflex.Channel
  , GLFWHost
  , hostGLFW
    -- * Creating windows
  , WindowE (..)
  , CreateWindow (..)
  , WindowAction (..)
  , Reflex.Host.GLFW.createWindow
    -- ** Overloading window callbacks
  , module Reflex.Host.GLFW.Window
    -- * Re-exports
    --
    -- | "Graphics.UI.GLFW.Reexport" drops 'GLFW.createWindow', 'GLFW.waitEvents',
    --   'GLFW.pollEvents', and all the callbacks and their corresponding types.
    --
    --   You probably want to lookup documentation in "Graphics.UI.GLFW" instead since
    --   the re-export does not follow the export list.
  , module Graphics.UI.GLFW.Reexport
  ) where

import           Graphics.UI.GLFW.Reexport
import           Reflex.Channel
import           Reflex.Host.GLFW.Internal
import           Reflex.Host.GLFW.Window

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Ref
import           Data.Bifunctor.Flip
import           Data.Dependent.Sum
import           Data.Foldable
import           Data.Maybe
import           Data.Proxy
import           Data.Traversable
import           Graphics.UI.GLFW as GLFW
import           Reflex
import           Reflex.Host.Class



-- | Global GLFW callbacks.
--
--   Consider using @{-\# LANGUAGE RecordWildCards \#-}@.
data GlobalE t =
       GlobalE
         { postBuildE :: Event t ()
         , errorE     :: Event t (GLFW.Error, [Char])
         , monitorE   :: Event t (Monitor, MonitorState)
         , joystickE  :: Event t (Joystick, JoystickState)
         }



-- | 'GLFW.createWindow' counterpart. One event creates a window, the other one destroys.
--   
--   If the creation 'Event' happens with a 'Window' already existing, it is ignored.
--   Same with destroying a window after it has already been destroyed.
--
--   'Window' can be created anew after destruction.
--
--   'WindowE' callback events will proc based on the current 'Window',
--   or 'never' if none exists currently.
--
--   You might require @{-\# LANGUAGE RecursiveDo \#-}@ to tie 'Event's and
--   window destruction together.
createWindow
  :: ( SpiderTimeline Global ~ t
     )
  => HostChannel
  -> Event t CreateWindow         -- ^ Window creation event
  -> Event t ()                   -- ^ Window destruction event
  -> GLFWHost ( Event t WindowAction
              , Behavior t (Maybe Window)
              , WindowE Def t
              )
createWindow = createWindow' Proxy



-- | Hosts @GLFW@ using @reflex@.
--
--   This does not run 'GLFW.init' or 'GLFW.terminate', so @GLFW@ is expected
--   to be initialized before calling 'hostGLFW' and terminated after. No window
--   contexts are set up or cleaned up either.
--
--   The output 'Event' quits 'hostGLFW'.
--
--   === MULTITHREADING
--
--     Trigger Events are run on an __unbound__ thread. Therefore:
--
--       * Any GLFW functions that have a \"must only be called from the main thread\"
--         restriction should be bounced onto the 'HostChannel' using 'PerformEventOn'.
--
--       * If you wish to run context-dependent libraries, like OpenGL or OpenAL,
--         you should either use 'newBoundChannel', or constantly rebind the context
--         on every fork.
hostGLFW
  :: ( SpiderTimeline Global ~ t
     , SpiderHost Global ~ h
     )
  => ( HostChannel
    -> GlobalE t
    -> GLFWHost (Event t ())
     )
  -> IO ()
hostGLFW network =
  runSpiderHost $ mdo

    -- Set up event hooks
    (postBuildE , postBuildRef) <- newEventWithTriggerRef

    (errorE     ,     errorRef) <- newEventWithTriggerRef
    (monitorE   ,   monitorRef) <- newEventWithTriggerRef
    (joystickE  ,  joystickRef) <- newEventWithTriggerRef

    -- Channel for triggering events in another thread
    triggerChan <- liftIO newChan
    -- Channel for relaying events back onto the main thread
    hostChan <- liftIO newTQueueIO

    (shutdownE, FireCommand fireCommand)
      <- hostPerformEventT
           . flip runPostBuildT postBuildE
           . flip runTriggerEventT triggerChan
           $ network (HostChannel hostChan) GlobalE {..}
 
    shutdownH <- subscribeEvent shutdownE

    quitRef <- newRef False

    -- Set up all the non-window callbacks
    liftIO $ do
      setup triggerChan    errorRef setErrorCallback    return $ \f e s -> f (e, s)
      setup triggerChan  monitorRef setMonitorCallback  return $ \f m s -> f (m, s)
      setup triggerChan joystickRef setJoystickCallback return $ \f j s -> f (j, s)

    pbRef <- readRef postBuildRef
    for_ pbRef $ \t ->
      fireCommand [t ==> ()] $ return ()

    triggerThread <-
      liftIO . forkIO . forever $ do

        events <- readChan triggerChan

        runSpiderHost $ do
          evs <- liftIO .
                   for events $ \(EventTriggerRef evRef :=> TriggerInvocation val _) -> do
                     ev <- readRef evRef
                     return $ fmap (==> val) ev

          toShutdown <- fireCommand (catMaybes evs) $ readEvent shutdownH

          liftIO .
            for_ events $ \(_ :=> TriggerInvocation _ callback) ->
              callback

          when (any isJust toShutdown) $ do
            writeRef quitRef True
            liftIO postEmptyEvent

    liftIO .
      finally
        ( fix $ \loop -> do

            eventList <- atomically $ flushTQueue hostChan

            for_ eventList $ \events ->
              for_ events $ \(Flip outRef :=> io) ->
                io >>= outRef

            -- Check whether we should quit
            hasQuit <- readRef quitRef

            unless hasQuit $ do
              GLFW.waitEvents
              loop
        )
        -- Cleanup after the program quits
        $ killThread triggerThread
