{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Host.GLFW
  ( -- * Host
    GlobalE (..)
  , WindowE (..)
  , Fire
  , CreateWindow (..)
  , Reflex.Host.GLFW.createWindow
  , createWindows
  , hostGLFW
    -- * Re-exports
    --
    -- | "Graphics.UI.GLFW.Reexport" drops 'GLFW.createWindow', 'GLFW.waitEvents',
    --   'GLFW.pollEvents', and all the callbacks and their corresponding types.
    --
    --   You probably want to lookup documentation in "Graphics.UI.GLFW" instead since it's
    --   not following the export list in the re-export.
  , createWindow'
  , module Graphics.UI.GLFW.Reexport
  ) where

import           Graphics.UI.GLFW.Reexport
import           Reflex.Host.GLFW.Internal

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Ref
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



-- | 'GLFW.createWindow' counterpart. One event creates a window, the other one destroys.
--   
--   If the creation 'Event' happens with a 'Window' already existing, it is ignored.
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
     , SpiderHost Global ~ h
     , MonadHold t m
     , MonadIO (Performable m)
     , MonadFix m
     , PerformEvent t m
     , Reflex t
     , TriggerEvent t m
     )
  => Fire t h a
  -> Event t CreateWindow         -- ^ Window creation event
  -> Event t ()                   -- ^ Window destruction event
  -> m ( Dynamic t (Maybe Window)
       , WindowE Identity t
       )
createWindow (Fire fire) (bearE :: Event t CreateWindow) killE = mdo

  createdE <- performEvent $
                gate (isNothing <$> windowB) bearE <&>
                  \(CreateWindow s t m w) -> liftIO $ hostWindow fire s t m w

  destroyedE <- performEvent $ liftIO . GLFW.destroyWindow <$> tagMaybe windowB killE

  -- Dynamic t (Maybe (Window, WindowE Identity t))
  windowDE <- holdDyn Nothing $ leftmost
                                  [ createdE
                                  , Nothing <$ destroyedE
                                  ]

  let windowD = fmap fst <$> windowDE
      windowB = current windowD
      -- A behavior with all the callbacks
      windowBE = fmap snd <$> current windowDE

  return
    ( windowD
    , let grab :: (WindowE Identity t -> Event t a) -> Event t a
          grab f = switch $ fromMaybe never . fmap f <$> windowBE
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



data WindowAction = CouldNotCreateWindow
                  | CreatedWindow GLFW.Window
                  | DestroyedWindow GLFW.Window
                  | DestroyedAllWindows (Set GLFW.Window)
                    deriving Show

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
     , Reflex t
     , TriggerEvent t m
     )
  => Fire t h a
  -> Event t CreateWindow        -- ^ Window creation event
  -> Event t (Maybe Window)      -- ^ Window destruction event. 'Nothing' destroys all windows.
  -> m ( Event t WindowAction
       , Behavior t (Set Window)
       , WindowE (Map Window) t
       )
createWindows (Fire fire) (bearE :: Event t CreateWindow) killE = mdo

  createdE <- performEvent $
                bearE <&>
                  \(CreateWindow s t m w) -> liftIO $ hostWindow fire s t m w

  destroyedE <- performEvent $
                  attach windowBE killE <&>
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

  windowDE <- accumDyn (foldl' pick) Map.empty
                $ mergeList
                    [ Right <$> createdE
                    , Left <$> destroyedE
                    ]

  let windowBE = Map.keysSet <$> current windowDE

  return
    ( leftmost
        [ createdE <&>
            \mayWindow ->
              case mayWindow of
                Nothing     -> CouldNotCreateWindow
                Just (w, _) -> CreatedWindow w
        , attach windowBE destroyedE <&>
            \(windows, mayWindow) ->
              case mayWindow of
                Nothing -> DestroyedAllWindows windows
                Just w  -> DestroyedWindow w
        ]
    , windowBE
    , let grab :: (WindowE Identity t -> Event t a) -> Event t (Map Window a)
          grab f = switch $ mergeMap . fmap f <$> current windowDE
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



-- | Hosts @GLFW@ using @reflex@.
--
--   This does not run 'GLFW.init' or 'GLFW.terminate', so @GLFW@ is expected
--   to be initialized before calling 'hostGLFW' and terminated after.
--
--   The output event quits 'hostGLFW', which sets the context on this thread to 'Nothing'
--   and stops processing any events after the frame that called it.
--
--   'PerformEvent' usages will execute 'IO' actions on the same thread that called 'hostGLFW',
--   therefore staying consistent with the \"must only be called from the main thread\" restriction
--   put on most functions by the @GLFW@ spec.
--
--   Internally 'GLFW.waitEvents' is used for event polling, so if you wish to do something
--   @GLFW@-specific based on an event not caused by @GLFW@ you will need to 'postEmptyEvent'.
hostGLFW
  :: ( SpiderTimeline Global ~ t
     , SpiderHost Global ~ h
     )
  => (Fire t h () -> GlobalE t -> TriggerEventT t (PostBuildT t (PerformEventT t h)) (Event t ()))
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

    (outputE, fireCommand)
      <- hostPerformEventT
           . flip runPostBuildT postBuildE
           . flip runTriggerEventT triggerChan
           $ network (Fire fire) GlobalE {..}
 
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
            -- Check whether we should quit
            hasQuit <- readRef quitRef

            unless hasQuit $ do
              GLFW.waitEvents
              loop
        )
        -- Cleanup after the program quits
        $ do makeContextCurrent Nothing
             killThread triggerThread


-- | 'GLFW.createWindow' re-export in the case you wish to create a window without setting up the callbacks.
createWindow' :: Int -> Int -> String -> Maybe Monitor -> Maybe Window -> IO (Maybe Window)
createWindow' = GLFW.createWindow
