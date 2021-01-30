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
  , Fire
  , HostChan
  , GLFWHost
  , hostGLFW
    -- * Creating windows
  , WindowE (..)
  , CreateWindow (..)
  , Count
  , WindowAction (..)
  , Reflex.Host.GLFW.createWindow
  , createWindows
    -- * Triggering events on the host
  , newHostEvent
  , newHostEventWithOnComplete
  , newEventWithLazyHostWithOnComplete
    -- * Re-exports
    --
    -- | "Graphics.UI.GLFW.Reexport" drops 'GLFW.createWindow', 'GLFW.waitEvents',
    --   'GLFW.pollEvents', and all the callbacks and their corresponding types.
    --
    --   You probably want to lookup documentation in "Graphics.UI.GLFW" instead since
    --   the re-export does not follow the export list.
  , createWindow'
  , module Graphics.UI.GLFW.Reexport
  ) where

import           Graphics.UI.GLFW.Reexport
import           Reflex.Host.GLFW.Internal

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Ref
import           Control.Monad.STM
import           Data.Dependent.Sum
import           Data.Foldable
import           Data.Functor ((<&>))
import           Data.Functor.Identity
import           Data.Maybe (fromMaybe, isNothing, isJust)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
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



-- | Wrapped 'GLFW.createWindow' arguments to pass through an event.
data CreateWindow = CreateWindow (Int, Int) String (Maybe Monitor) (Maybe Window)



data Count = One
           | Many

-- | Types of window events.
data WindowAction (a :: Count) where
  CouldNotCreateWindow :: WindowAction a
  CreatedWindow        :: Window       -> WindowAction a
  DestroyedWindow      :: Window       -> WindowAction a
  DestroyedAllWindows  :: Set Window   -> WindowAction 'Many

deriving instance Show (WindowAction 'One)
deriving instance Show (WindowAction 'Many)


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
--   All output events happen on the host thread.
--
--   You might require @{-\# LANGUAGE RecursiveDo \#-}@ to tie 'Event's and
--   window destruction together.
createWindow
  :: ( SpiderTimeline Global ~ t
     , SpiderHost Global ~ h
     , MonadHold t m
     , MonadIO (Performable m)
     , MonadFix m
     , PerformEvent t m
     , TriggerEvent t m
     )
  => Fire t h a
  -> Event t CreateWindow         -- ^ Window creation event
  -> Event t ()                   -- ^ Window destruction event
  -> m ( Event t (WindowAction 'One)
       , Behavior t (Maybe Window)
       , WindowE Identity t
       )
createWindow (Fire fire) (bearE :: Event t CreateWindow) killE = mdo

  createdE <- performEvent $
                gate (isNothing <$> windowB) bearE <&>
                  \(CreateWindow s t m w) -> liftIO $ hostWindow fire s t m w

  destroyedE <- performEvent $ tagMaybe windowB killE
                                 <&> \window -> do
                                        liftIO $ GLFW.destroyWindow window
                                        return window

  windowBE <- hold Nothing $ leftmost
                               [ createdE
                               , Nothing <$ destroyedE
                               ]

  let windowB = fmap fst <$> windowBE
      callbacksB = fmap snd <$> windowBE

  return
    ( leftmost
        [ createdE <&> \mayWindow ->
                         case mayWindow of
                           Nothing     -> CouldNotCreateWindow
                           Just (w, _) -> CreatedWindow w
        , DestroyedWindow <$> destroyedE
        ]
    , windowB
    , let grab :: (WindowE Identity t -> Event t a) -> Event t a
          grab f = switch $ fromMaybe never . fmap f <$> callbacksB
      in WindowE
           { windowPositionE     = grab windowPositionE
           , windowSizeE         = grab windowSizeE
           , windowCloseE        = grab windowCloseE
           , windowRefreshE      = grab windowRefreshE
           , windowFocusE        = grab windowFocusE
           , windowIconifyE      = grab windowIconifyE
           , framebufferSizeE    = grab framebufferSizeE
#if MIN_VERSION_GLFW_b(3,3,0)
           , windowContentScaleE = grab windowContentScaleE
           , windowMaximizeE     = grab windowMaximizeE
#endif
           , keyE                = grab keyE
           , charE               = grab charE
           , charModsE           = grab charModsE
           , mouseButtonE        = grab mouseButtonE
           , cursorE             = grab cursorE
           , cursorEnterE        = grab cursorEnterE
           , scrollE             = grab scrollE
           , dropE               = grab dropE
           }
    )



-- | A variation of 'Reflex.Host.GLFW.createWindows' that holds a 'Set' of 'Window's.
--
--   'WindowE' combines events for all windows initialized.
createWindows
  :: ( SpiderTimeline Global ~ t
     , SpiderHost Global ~ h
     , MonadHold t m
     , MonadIO (Performable m)
     , MonadFix m
     , PerformEvent t m
     , TriggerEvent t m
     )
  => Fire t h a
  -> Event t CreateWindow        -- ^ Window creation event
  -> Event t (Maybe Window)      -- ^ Window destruction event. 'Nothing' destroys all windows.
  -> m ( Event t (WindowAction 'Many)
       , Behavior t (Set Window)
       , WindowE (Map Window) t
       )
createWindows (Fire fire) (bearE :: Event t CreateWindow) killE = mdo

  createdE <- performEvent $
                bearE <&>
                  \(CreateWindow s t m w) -> liftIO $ hostWindow fire s t m w

  destroyedE <- performEvent $
                  attach windowB killE <&>
                    \(windows, mayWindow) -> do
                       liftIO $
                         case mayWindow of
                           Nothing     -> mapM_ GLFW.destroyWindow windows
                           Just window -> GLFW.destroyWindow window
                       return mayWindow

  let pick ws action =
        case action of
          Right Nothing        -> ws
          Right (Just (w, wE)) -> Map.insert w wE ws
          Left Nothing         -> Map.empty
          Left (Just w)        -> Map.delete w    ws

  windowBE <- accum (foldl' pick) Map.empty
                $ mergeList
                    [ Right <$> createdE
                    , Left <$> destroyedE
                    ]

  let windowB = Map.keysSet <$> windowBE

  return
    ( leftmost
        [ createdE <&>
            \mayWindow ->
              case mayWindow of
                Nothing     -> CouldNotCreateWindow
                Just (w, _) -> CreatedWindow w
        , attach windowB destroyedE <&>
            \(windows, mayWindow) ->
              case mayWindow of
                Nothing -> DestroyedAllWindows windows
                Just w  -> DestroyedWindow w
        ]
    , windowB
    , let grab :: (WindowE Identity t -> Event t a) -> Event t (Map Window a)
          grab f = switch $ mergeMap . fmap f <$> windowBE
      in WindowE
           { windowPositionE     = grab windowPositionE
           , windowSizeE         = grab windowSizeE
           , windowCloseE        = grab windowCloseE
           , windowRefreshE      = grab windowRefreshE
           , windowFocusE        = grab windowFocusE
           , windowIconifyE      = grab windowIconifyE
           , framebufferSizeE    = grab framebufferSizeE
#if MIN_VERSION_GLFW_b(3,3,0)
           , windowContentScaleE = grab windowContentScaleE
           , windowMaximizeE     = grab windowMaximizeE
#endif
           , keyE                = grab keyE
           , charE               = grab charE
           , charModsE           = grab charModsE
           , mouseButtonE        = grab mouseButtonE
           , cursorE             = grab cursorE
           , cursorEnterE        = grab cursorEnterE
           , scrollE             = grab scrollE
           , dropE               = grab dropE
           }
    )



-- | Shorthand.
type GLFWHost = TriggerEventT (SpiderTimeline Global)
                  ( PostBuildT (SpiderTimeline Global)
                      ( PerformEventT (SpiderTimeline Global) (SpiderHost Global)
                      )
                  )

-- | Hosts @GLFW@ using @reflex@.
--
--   This does not run 'GLFW.init' or 'GLFW.terminate', so @GLFW@ is expected
--   to be initialized before calling 'hostGLFW' and terminated after. No window
--   contexts are set up or cleaned up either.
--
--   The output event quits 'hostGLFW', which stops processing any events after
--   the frame that called it.
--
--   === MULTITHREADING
--
--     Most @GLFW@ functions have a \"must only be called from
--     the main thread\" restriction on them. Running these from a different thread,
--     while not mission critical for some OSs, might cause crashes on other ones.
--     'hostGLFW' runs two threads:
--
--     * The one you ran 'hostGLFW' on. 'createWindow', 'createWindows', 'GlobalE' and
--       'WindowE' events run on this thread. It must be a
--       [bound](hackage.haskell.org/package/base/docs/Control-Concurrent.html#g:8)
--       thread (main thread is always bound).
--
--     * The secondary __unbound__ 'TriggerEvent' thread. Any 'Event' created using
--       'newTriggerEvent' will be emitted by this thread. As it is unbound you should
--       not use it to run OpenGL or OpenAL, you should probably create additional bound
--       threads for it using 'MonadReflexCreateTrigger'
--       (see 'TriggerEvent' implementation, specifically the 'TriggerEventT' instance).
--
--     'PerformEvent' executes operations on the same thread as the event that caused it.
--     If you wish to do something on the host thread from an event running on the
--     secondary thread, please use 'newHostEvent'.
hostGLFW
  :: ( SpiderTimeline Global ~ t
     , SpiderHost Global ~ h
     )
  => ( Fire t h ()
    -> HostChan t
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
    hostChan <- liftIO newTChanIO

    (outputE, fireCommand)
      <- hostPerformEventT
           . flip runPostBuildT postBuildE
           . flip runTriggerEventT triggerChan
           $ network (Fire fire) (HostChan hostChan) GlobalE {..}
 
    outputH <- subscribeEvent outputE

    quitRef <- liftIO $ newRef False

    let fire a = do
          output <- runFireCommand fireCommand a $
                      readEvent outputH
          
          when (any isJust output) $
            writeRef quitRef True

    -- Set up all the non-window callbacks
    setup fire    errorRef setErrorCallback    $ \f e s -> f (e, s)
    setup fire  monitorRef setMonitorCallback  $ \f m s -> f (m, s)
    setup fire joystickRef setJoystickCallback $ \f j s -> f (j, s)

    triggerThread <-
      liftIO . forkIO . forever $ do
        -- Read events, fire all of them and then call all the callbacks.
        events <- liftIO $ readChan triggerChan
        for_ events $ \(EventTriggerRef evRef :=> TriggerInvocation val _) -> do
          ev <- readRef evRef
          for_ ev $ \t -> do
            runSpiderHost $ fire [t :=> Identity val]

        for_ events $ \(_ :=> TriggerInvocation _ callback) ->
          callback

    -- Fire a postBuild Event right before going into a fix
    pbRef <- readRef postBuildRef
    for_ pbRef $ \t ->
      fire [t :=> Identity ()]

    liftIO .
      finally
        ( fix $ \loop -> do

            -- Fire all of events this thread got
            fix $ \loopChan -> do
              mayEvents <- atomically $ tryReadTChan hostChan
              for_ mayEvents $ \events -> do

                for_ events $ \(EventTriggerRef evRef :=> TriggerInvocation val _) -> do
                  ev <- readRef evRef
                  for_ ev $ \t -> do
                    runSpiderHost $ fire [t :=> Identity val]

                  for_ events $ \(_ :=> TriggerInvocation _ callback) ->
                    callback

                  loopChan

            -- Check whether we should quit
            hasQuit <- readRef quitRef

            unless hasQuit $ do
              GLFW.waitEvents
              loop
        )
        -- Cleanup after the program quits
        $ killThread triggerThread



-- | Analogous to 'newTriggerEvent', except the 'Event' is fired by the host thread.
newHostEvent
  :: ( MonadRef m
     , Ref m ~ Ref IO
     , MonadReflexCreateTrigger t m
     )
  => HostChan t
  -> m (Event t a, a -> IO ())
newHostEvent hostChan = do
  (e, t) <- newHostEventWithOnComplete hostChan
  return (e, \a -> t a $ return ())

-- | Analogous to 'newTriggerEventWithOnComplete', except the 'Event' is fired by the
--   host thread.
newHostEventWithOnComplete
  :: ( MonadRef m
     , Ref m ~ Ref IO
     , MonadReflexCreateTrigger t m
     )
  => HostChan t
  -> m (Event t a, a -> IO () -> IO ())
newHostEventWithOnComplete (HostChan events) = do
  (res, resRef) <- newEventWithTriggerRef
  return . (,) res $ \a cb -> do
    atomically $ writeTChan events [EventTriggerRef resRef :=> TriggerInvocation a cb]
    postEmptyEvent

-- | Analogous to 'newEventWithLazyHostWithOnComplete', except the 'Event' is fired by the
--   host thread.
newEventWithLazyHostWithOnComplete
  :: ( MonadRef m    
     , Ref m ~ Ref IO
     , MonadReflexCreateTrigger t m
     )
  => HostChan t
  -> ( (a -> IO () -> IO ())
    -> IO (IO ())
     )
  -> m (Event t a)
newEventWithLazyHostWithOnComplete (HostChan events) f = do
  newEventWithTrigger $ \t ->
    f $ \a cb -> do
      resRef <- newRef $ Just t
      atomically $ writeTChan events [EventTriggerRef resRef :=> TriggerInvocation a cb]
      postEmptyEvent



-- | 'GLFW.createWindow' re-export in the case you wish to create a window without setting up the callbacks.
createWindow' :: Int -> Int -> String -> Maybe Monitor -> Maybe Window -> IO (Maybe Window)
createWindow' = GLFW.createWindow
