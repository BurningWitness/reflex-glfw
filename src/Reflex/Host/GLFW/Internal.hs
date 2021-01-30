{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK not-home #-}

module Reflex.Host.GLFW.Internal where

import           Control.Concurrent.STM.TChan
import           Control.Monad.IO.Class
import           Control.Monad.Ref
import           Data.Dependent.Sum
import           Data.Foldable (for_)
import           Data.Functor.Identity
import           Data.Traversable (forM)
import           Graphics.UI.GLFW
import           Reflex
import           Reflex.Host.Class



type family NoIdentity f a :: * where
  NoIdentity Identity a = a
  NoIdentity f        a = f a

-- | Per-window GLFW callbacks. 'NoIdentity' here just removes 'Identity' from definitions.
--
--   Consider using @{-\# LANGUAGE RecordWildCards \#-}@ if you're using only a single window.
data WindowE f t =
       WindowE
         { windowPositionE     :: Event t (NoIdentity f (Window, Int, Int))
         , windowSizeE         :: Event t (NoIdentity f (Window, Int, Int))
         , windowCloseE        :: Event t (NoIdentity f  Window)
         , windowRefreshE      :: Event t (NoIdentity f  Window)
         , windowFocusE        :: Event t (NoIdentity f (Window, Bool))
         , windowIconifyE      :: Event t (NoIdentity f (Window, Bool))
         , framebufferSizeE    :: Event t (NoIdentity f (Window, Int, Int))
#if MIN_VERSION_GLFW_b(3,3,0)
         , windowContentScaleE :: Event t (NoIdentity f (Window, Float, Float))
         , windowMaximizeE     :: Event t (NoIdentity f (Window, Bool))
#endif
         , keyE                :: Event t (NoIdentity f (Window, Key, Int, KeyState, ModifierKeys))
         , charE               :: Event t (NoIdentity f (Window, Char))
         , charModsE           :: Event t (NoIdentity f (Window, Char, ModifierKeys))
         , mouseButtonE        :: Event t (NoIdentity f (Window, MouseButton, MouseButtonState, ModifierKeys))
         , cursorE             :: Event t (NoIdentity f (Window, Double, Double))
         , cursorEnterE        :: Event t (NoIdentity f (Window, CursorState))
         , scrollE             :: Event t (NoIdentity f (Window, Double, Double))
         , dropE               :: Event t (NoIdentity f (Window, [FilePath]))
         }



-- | A wrapper around the \"fire\" command.
newtype Fire t h a = Fire ([DSum (EventTrigger t) Identity] -> h a)



-- | A wrapper around the host channel, analogous to one inside 'TriggerEventT'.
newtype HostChan t = HostChan { unHostChan :: TChan [DSum (EventTriggerRef t) TriggerInvocation] }



-- | Setups up a callback. For uses look at 'hostWindow'.
setup
  :: ( SpiderTimeline Global ~ t
     , SpiderHost Global ~ h
     )
  => ([DSum (EventTrigger t) Identity] -> h c)
  -> Ref IO (Maybe (EventTrigger t a))
  -> (Maybe b -> IO ())
  -> ((a -> IO ()) -> b)
  -> h ()
setup fire ref callback shrink = do
  liftIO . callback . Just . shrink $ \b -> do
    res <- readRef ref
    for_ res $ \t ->
      runSpiderHost $ fire [t ==> b]



-- | Initializes all the callbacks if a window was created successfully.
hostWindow
  :: ( SpiderTimeline Global ~ t
     , SpiderHost Global ~ h
     )
  => ([DSum (EventTrigger t) Identity] -> h c)
  -> (Int, Int)
  -> String
  -> Maybe Monitor
  -> Maybe Window
  -> IO (Maybe (Window, WindowE Identity t))
hostWindow fire (width, height) title mayMonitor mayContext = do
  mayWindow <- createWindow width height title mayMonitor mayContext
  mayWindowE <-
    forM mayWindow $ \window ->
      runSpiderHost $ do
        (windowPositionE     ,          windowPosRef) <- newEventWithTriggerRef
        (windowSizeE         ,         windowSizeRef) <- newEventWithTriggerRef
        (windowCloseE        ,        windowCloseRef) <- newEventWithTriggerRef
        (windowRefreshE      ,      windowRefreshRef) <- newEventWithTriggerRef
        (windowFocusE        ,        windowFocusRef) <- newEventWithTriggerRef
        (windowIconifyE      ,      windowIconifyRef) <- newEventWithTriggerRef
        (framebufferSizeE    ,    framebufferSizeRef) <- newEventWithTriggerRef
#if MIN_VERSION_GLFW_b(3,3,0)
        (windowContentScaleE , windowContentScaleRef) <- newEventWithTriggerRef
        (windowMaximizeE     ,     windowMaximizeRef) <- newEventWithTriggerRef
#endif
        (keyE                ,                keyRef) <- newEventWithTriggerRef
        (charE               ,               charRef) <- newEventWithTriggerRef
        (charModsE           ,           charModsRef) <- newEventWithTriggerRef
        (mouseButtonE        ,        mouseButtonRef) <- newEventWithTriggerRef
        (cursorE             ,          cursorPosRef) <- newEventWithTriggerRef
        (cursorEnterE        ,        cursorEnterRef) <- newEventWithTriggerRef
        (scrollE             ,             scrollRef) <- newEventWithTriggerRef
        (dropE               ,               dropRef) <- newEventWithTriggerRef

        setup fire          windowPosRef (setWindowPosCallback          window) $ \f w x y     -> f (w, x, y)
        setup fire         windowSizeRef (setWindowSizeCallback         window) $ \f w x y     -> f (w, x, y)
        setup fire        windowCloseRef (setWindowCloseCallback        window) $ \f w         -> f w
        setup fire      windowRefreshRef (setWindowRefreshCallback      window) $ \f w         -> f w
        setup fire        windowFocusRef (setWindowFocusCallback        window) $ \f w b       -> f (w, b)
        setup fire      windowIconifyRef (setWindowIconifyCallback      window) $ \f w b       -> f (w, b)
        setup fire    framebufferSizeRef (setFramebufferSizeCallback    window) $ \f w x y     -> f (w, x, y)
#if MIN_VERSION_GLFW_b(3,3,0)
        setup fire windowContentScaleRef (setWindowContentScaleCallback window) $ \f w x y     -> f (w, x, y)
        setup fire     windowMaximizeRef (setWindowMaximizeCallback     window) $ \f w b       -> f (w, b)
#endif
        setup fire                keyRef (setKeyCallback                window) $ \f w k i s m -> f (w, k, i, s, m)
        setup fire               charRef (setCharCallback               window) $ \f w c       -> f (w, c)
        setup fire           charModsRef (setCharModsCallback           window) $ \f w c m     -> f (w, c, m)
        setup fire        mouseButtonRef (setMouseButtonCallback        window) $ \f w b s m   -> f (w, b, s, m)
        setup fire          cursorPosRef (setCursorPosCallback          window) $ \f w x y     -> f (w, x, y)
        setup fire        cursorEnterRef (setCursorEnterCallback        window) $ \f w s       -> f (w, s)
        setup fire             scrollRef (setScrollCallback             window) $ \f w x y     -> f (w, x, y)
        setup fire               dropRef (setDropCallback               window) $ \f w p       -> f (w, p)

        return WindowE {..}

  return $ (,) <$> mayWindow <*> mayWindowE
