{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Host.GLFW.Window
  ( WindowE (..)
  , CreateWindow (..)
  , WindowAction (..)
  , Def
  , Conversion (..)
  , createWindow'
  ) where

import           Reflex.Channel
import           Reflex.Host.GLFW.Internal

import           Control.Concurrent.Chan
import           Control.Monad.IO.Class
import           Data.Dependent.Sum
import           Data.Maybe
import           Data.Proxy
import           Data.Traversable
import           Graphics.UI.GLFW as GLFW
import           Reflex
import           Reflex.Host.Class



-- | Per-window GLFW callbacks.
--
--   Consider using @{-\# LANGUAGE RecordWildCards \#-}@ if you're using only a single window.
data WindowE c t =
       WindowE
         { windowPosE          :: Event t (WindowPosE          c)
         , windowSizeE         :: Event t (WindowSizeE         c)
         , windowCloseE        :: Event t (WindowCloseE        c)
         , windowRefreshE      :: Event t (WindowRefreshE      c)
         , windowFocusE        :: Event t (WindowFocusE        c)
         , windowIconifyE      :: Event t (WindowIconifyE      c)
         , framebufferSizeE    :: Event t (FramebufferSizeE    c)
#if MIN_VERSION_GLFW_b(3,3,0)
         , windowContentScaleE :: Event t (WindowContentScaleE c)
         , windowMaximizeE     :: Event t (WindowMaximizeE     c)
#endif
         , keyE                :: Event t (KeyE                c)
         , charE               :: Event t (CharE               c)
         , charModsE           :: Event t (CharModsE           c)
         , mouseButtonE        :: Event t (MouseButtonE        c)
         , cursorPosE          :: Event t (CursorPosE          c)
         , cursorEnterE        :: Event t (CursorEnterE        c)
         , scrollE             :: Event t (ScrollE             c)
         , dropE               :: Event t (DropE               c)
         }



-- | Collection of per-window callback types and conversion functions from 'Def' ones to them.
--
--   When making a new one you most probably want to copy the 'Def' instance and replace
--   the types you wish to extend.
class Conversion c where
  type WindowPosE          c :: *
  type WindowSizeE         c :: *
  type WindowCloseE        c :: *
  type WindowRefreshE      c :: *
  type WindowFocusE        c :: *
  type WindowIconifyE      c :: *
  type FramebufferSizeE    c :: *
#if MIN_VERSION_GLFW_b(3,3,0)
  type WindowContentScaleE c :: *
  type WindowMaximizeE     c :: *
#endif
  type KeyE                c :: *
  type CharE               c :: *
  type CharModsE           c :: *
  type MouseButtonE        c :: *
  type CursorPosE          c :: *
  type CursorEnterE        c :: *
  type ScrollE             c :: *
  type DropE               c :: *

  windowPosConv          :: Proxy c -> WindowPosE          Def -> IO (WindowPosE          c)
  windowSizeConv         :: Proxy c -> WindowSizeE         Def -> IO (WindowSizeE         c)
  windowCloseConv        :: Proxy c -> WindowCloseE        Def -> IO (WindowCloseE        c)
  windowRefreshConv      :: Proxy c -> WindowRefreshE      Def -> IO (WindowRefreshE      c)
  windowFocusConv        :: Proxy c -> WindowFocusE        Def -> IO (WindowFocusE        c)
  windowIconifyConv      :: Proxy c -> WindowIconifyE      Def -> IO (WindowIconifyE      c)
  framebufferSizeConv    :: Proxy c -> FramebufferSizeE    Def -> IO (FramebufferSizeE    c)
#if MIN_VERSION_GLFW_b(3,3,0)
  windowContentScaleConv :: Proxy c -> WindowContentScaleE Def -> IO (WindowContentScaleE c)
  windowMaximizeConv     :: Proxy c -> WindowMaximizeE     Def -> IO (WindowMaximizeE     c)
#endif
  keyConv                :: Proxy c -> KeyE                Def -> IO (KeyE                c)
  charConv               :: Proxy c -> CharE               Def -> IO (CharE               c)
  charModsConv           :: Proxy c -> CharModsE           Def -> IO (CharModsE           c)
  mouseButtonConv        :: Proxy c -> MouseButtonE        Def -> IO (MouseButtonE        c)
  cursorPosConv          :: Proxy c -> CursorPosE          Def -> IO (CursorPosE          c)
  cursorEnterConv        :: Proxy c -> CursorEnterE        Def -> IO (CursorEnterE        c)
  scrollConv             :: Proxy c -> ScrollE             Def -> IO (ScrollE             c)
  dropConv               :: Proxy c -> DropE               Def -> IO (DropE               c)



-- | Type constructor holding default window event types.
data Def

instance Conversion Def where
  type WindowPosE          Def = (Window, Int, Int)
  type WindowSizeE         Def = (Window, Int, Int)
  type WindowCloseE        Def =  Window
  type WindowRefreshE      Def =  Window
  type WindowFocusE        Def = (Window, Bool)
  type WindowIconifyE      Def = (Window, Bool)
  type FramebufferSizeE    Def = (Window, Int, Int)
#if MIN_VERSION_GLFW_b(3,3,0)
  type WindowContentScaleE Def = (Window, Float, Float)
  type WindowMaximizeE     Def = (Window, Bool)
#endif
  type KeyE                Def = (Window, Key, Int, KeyState, ModifierKeys)
  type CharE               Def = (Window, Char)
  type CharModsE           Def = (Window, Char, ModifierKeys)
  type MouseButtonE        Def = (Window, MouseButton, MouseButtonState, ModifierKeys)
  type CursorPosE          Def = (Window, Double, Double)
  type CursorEnterE        Def = (Window, CursorState)
  type ScrollE             Def = (Window, Double, Double)
  type DropE               Def = (Window, [FilePath])

  windowPosConv          _ = return
  windowSizeConv         _ = return
  windowCloseConv        _ = return
  windowRefreshConv      _ = return
  windowFocusConv        _ = return
  windowIconifyConv      _ = return
  framebufferSizeConv    _ = return
#if MIN_VERSION_GLFW_b(3,3,0)
  windowContentScaleConv _ = return
  windowMaximizeConv     _ = return
#endif
  keyConv                _ = return
  charConv               _ = return
  charModsConv           _ = return
  mouseButtonConv        _ = return
  cursorPosConv          _ = return
  cursorEnterConv        _ = return
  scrollConv             _ = return
  dropConv               _ = return



-- | Wrapped 'GLFW.createWindow' arguments to pass through an event.
data CreateWindow = CreateWindow (Int, Int) String (Maybe Monitor) (Maybe Window)



-- | Types of window events.
data WindowAction = CouldNotCreateWindow
                  | CreatedWindow Window
                  | DestroyedWindow Window
                    deriving Show



-- | Initializes all the callbacks if a window was created successfully.
hostWindow
  :: ( SpiderTimeline Global ~ t
     , Conversion c
     )
  => Proxy c
  -> Chan [DSum (EventTriggerRef t) TriggerInvocation]
  -> (Int, Int)
  -> String
  -> Maybe Monitor
  -> Maybe Window
  -> IO (Maybe (Window, WindowE c t))
hostWindow conv chan (width, height) title mayMonitor mayContext = do
  mayWindow <- createWindow width height title mayMonitor mayContext
  mayWindowE <-
    forM mayWindow $ \window ->
      runSpiderHost $ do
        (windowPosE          ,          windowPosRef) <- newEventWithTriggerRef
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
        (cursorPosE          ,          cursorPosRef) <- newEventWithTriggerRef
        (cursorEnterE        ,        cursorEnterRef) <- newEventWithTriggerRef
        (scrollE             ,             scrollRef) <- newEventWithTriggerRef
        (dropE               ,               dropRef) <- newEventWithTriggerRef

        liftIO $ do
          setup chan          windowPosRef (setWindowPosCallback          window) (windowPosConv          conv) $ \f w x y     -> f (w, x, y)
          setup chan         windowSizeRef (setWindowSizeCallback         window) (windowSizeConv         conv) $ \f w x y     -> f (w, x, y)
          setup chan        windowCloseRef (setWindowCloseCallback        window) (windowCloseConv        conv) $ \f w         -> f w
          setup chan      windowRefreshRef (setWindowRefreshCallback      window) (windowRefreshConv      conv) $ \f w         -> f w
          setup chan        windowFocusRef (setWindowFocusCallback        window) (windowFocusConv        conv) $ \f w b       -> f (w, b)
          setup chan      windowIconifyRef (setWindowIconifyCallback      window) (windowIconifyConv      conv) $ \f w b       -> f (w, b)
          setup chan    framebufferSizeRef (setFramebufferSizeCallback    window) (framebufferSizeConv    conv) $ \f w x y     -> f (w, x, y)
#if MIN_VERSION_GLFW_b(3,3,0)
          setup chan windowContentScaleRef (setWindowContentScaleCallback window) (windowContentScaleConv conv) $ \f w x y     -> f (w, x, y)
          setup chan     windowMaximizeRef (setWindowMaximizeCallback     window) (windowMaximizeConv     conv) $ \f w b       -> f (w, b)
#endif
          setup chan                keyRef (setKeyCallback                window) (keyConv                conv) $ \f w k i s m -> f (w, k, i, s, m)
          setup chan               charRef (setCharCallback               window) (charConv               conv) $ \f w c       -> f (w, c)
          setup chan           charModsRef (setCharModsCallback           window) (charModsConv           conv) $ \f w c m     -> f (w, c, m)
          setup chan        mouseButtonRef (setMouseButtonCallback        window) (mouseButtonConv        conv) $ \f w b s m   -> f (w, b, s, m)
          setup chan          cursorPosRef (setCursorPosCallback          window) (cursorPosConv          conv) $ \f w x y     -> f (w, x, y)
          setup chan        cursorEnterRef (setCursorEnterCallback        window) (cursorEnterConv        conv) $ \f w s       -> f (w, s)
          setup chan             scrollRef (setScrollCallback             window) (scrollConv             conv) $ \f w x y     -> f (w, x, y)
          setup chan               dropRef (setDropCallback               window) (dropConv               conv) $ \f w p       -> f (w, p)

          return WindowE {..}

  return $ (,) <$> mayWindow <*> mayWindowE



-- | 'Reflex.Host.GLFW.createWindow' variant that allows for overloading callback types.
--
--   Any actions executed in the 'Conversion' functions happen directly inside the
--   callback functions, so if, for example, you're sampling time at which
--   an input happened, this is the fastest route.
createWindow'
  :: ( SpiderTimeline Global ~ t
     , Conversion c
     )
  => Proxy c                      -- ^ Conversion type
  -> HostChannel
  -> Event t CreateWindow         -- ^ Window creation event
  -> Event t ()                   -- ^ Window destruction event
  -> GLFWHost ( Event t WindowAction
              , Behavior t (Maybe Window)
              , WindowE c t
              )
createWindow' (conv :: Proxy c) hostChan (bearE :: Event t CreateWindow) killE = mdo

  chan <- askEvents

  createdE <- performEventOn hostChan $
                ( \(CreateWindow s t m w) -> liftIO $ hostWindow conv chan s t m w
                ) <$> gate (isNothing <$> windowB) bearE

  destroyedE <- performEventOn hostChan $
                  ( \window -> do
                      liftIO $ GLFW.destroyWindow window
                      return window
                  ) <$> tagMaybe windowB killE

  windowBE <- hold Nothing $ leftmost
                               [ createdE
                               , Nothing <$ destroyedE
                               ]

  let windowB = fmap fst <$> windowBE
      callbacksB = fmap snd <$> windowBE

  return
    ( leftmost
        [ ( \mayWindow ->
              case mayWindow of
                Nothing     -> CouldNotCreateWindow
                Just (w, _) -> CreatedWindow w
          ) <$> createdE
        , DestroyedWindow <$> destroyedE
        ]
    , windowB
    , let grab :: (WindowE c t -> Event t a) -> Event t a
          grab f = switch $ fromMaybe never . fmap f <$> callbacksB
      in WindowE
           { windowPosE          = grab windowPosE
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
           , cursorPosE          = grab cursorPosE
           , cursorEnterE        = grab cursorEnterE
           , scrollE             = grab scrollE
           , dropE               = grab dropE
           }
    )
