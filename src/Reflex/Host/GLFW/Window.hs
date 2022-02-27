{-# LANGUAGE CPP
           , DataKinds
           , DefaultSignatures
           , GADTs
           , PolyKinds
           , RecordWildCards
           , RecursiveDo
           , ScopedTypeVariables
           , TypeFamilies #-}

module Reflex.Host.GLFW.Window
  ( WindowE (..)
  , CreateWindow (..)
  , Def
  , Conversion (..)
  , createWindow'
  ) where

import           Reflex.Channel
import           Reflex.Host.GLFW.Internal

import           Control.Concurrent.Chan
import           Control.Monad.IO.Class
import           Data.Dependent.Sum
import           Data.Dependent.Map as DMap
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
  type WindowPosE          c = (Window, Int, Int)

  type WindowSizeE         c :: *
  type WindowSizeE         c = (Window, Int, Int)

  type WindowCloseE        c :: *
  type WindowCloseE        c =  Window

  type WindowRefreshE      c :: *
  type WindowRefreshE      c =  Window

  type WindowFocusE        c :: *
  type WindowFocusE        c = (Window, Bool)

  type WindowIconifyE      c :: *
  type WindowIconifyE      c = (Window, Bool)

  type FramebufferSizeE    c :: *
  type FramebufferSizeE    c = (Window, Int, Int)
#if MIN_VERSION_GLFW_b(3,3,0)
  type WindowContentScaleE c :: *
  type WindowContentScaleE c = (Window, Float, Float)

  type WindowMaximizeE     c :: *
  type WindowMaximizeE     c = (Window, Bool)
#endif
  type KeyE                c :: *
  type KeyE                c = (Window, Key, Int, KeyState, ModifierKeys)

  type CharE               c :: *
  type CharE               c = (Window, Char)

  type CharModsE           c :: *
  type CharModsE           c = (Window, Char, ModifierKeys)

  type MouseButtonE        c :: *
  type MouseButtonE        c = (Window, MouseButton, MouseButtonState, ModifierKeys)

  type CursorPosE          c :: *
  type CursorPosE          c = (Window, Double, Double)

  type CursorEnterE        c :: *
  type CursorEnterE        c = (Window, CursorState)

  type ScrollE             c :: *
  type ScrollE             c = (Window, Double, Double)

  type DropE               c :: *
  type DropE               c = (Window, [FilePath])


  windowPosConv :: Proxy c -> (Window, Int, Int) -> IO (WindowPosE c)
  default windowPosConv
             :: (Window, Int, Int) ~ WindowPosE c => Proxy c -> (Window, Int, Int) -> IO (WindowPosE c)
  windowPosConv          _ = return

  windowSizeConv :: Proxy c -> (Window, Int, Int) -> IO (WindowSizeE c)
  default windowSizeConv
            :: (Window, Int, Int) ~ WindowSizeE c => Proxy c -> (Window, Int, Int) -> IO (WindowSizeE c)
  windowSizeConv         _ = return

  windowCloseConv :: Proxy c -> Window -> IO (WindowCloseE c)
  default windowCloseConv :: Window ~ WindowCloseE c => Proxy c -> Window -> IO (WindowCloseE c)
  windowCloseConv        _ = return

  windowRefreshConv :: Proxy c -> Window -> IO (WindowRefreshE c)
  default windowRefreshConv :: Window ~ WindowRefreshE c => Proxy c -> Window -> IO (WindowRefreshE c)
  windowRefreshConv      _ = return

  windowFocusConv :: Proxy c -> (Window, Bool) -> IO (WindowFocusE c)
  default windowFocusConv
            :: (Window, Bool) ~ WindowFocusE c => Proxy c -> (Window, Bool) -> IO (WindowFocusE c)
  windowFocusConv        _ = return

  windowIconifyConv :: Proxy c -> (Window, Bool) -> IO (WindowIconifyE c)
  default windowIconifyConv
            :: (Window, Bool) ~ WindowIconifyE c => Proxy c -> (Window, Bool) -> IO (WindowIconifyE c)
  windowIconifyConv      _ = return

  framebufferSizeConv :: Proxy c -> (Window, Int, Int) -> IO (FramebufferSizeE c)
  default framebufferSizeConv
            :: (Window, Int, Int) ~ FramebufferSizeE c => Proxy c -> (Window, Int, Int) -> IO (FramebufferSizeE c)
  framebufferSizeConv    _ = return
#if MIN_VERSION_GLFW_b(3,3,0)
  windowContentScaleConv :: Proxy c -> (Window, Float, Float) -> IO (WindowContentScaleE c)
  default windowContentScaleConv
            :: (Window, Float, Float) ~ WindowContentScaleE c
            => Proxy c -> (Window, Float, Float) -> IO (WindowContentScaleE c)
  windowContentScaleConv _ = return

  windowMaximizeConv :: Proxy c -> (Window, Bool) -> IO (WindowMaximizeE c)
  default windowMaximizeConv
            :: (Window, Bool) ~ WindowMaximizeE c => Proxy c -> (Window, Bool) -> IO (WindowMaximizeE c)
  windowMaximizeConv     _ = return
#endif
  keyConv :: Proxy c -> (Window, Key, Int, KeyState, ModifierKeys) -> IO (KeyE c)
  default keyConv
            :: (Window, Key, Int, KeyState, ModifierKeys) ~ KeyE c
            => Proxy c -> (Window, Key, Int, KeyState, ModifierKeys) -> IO (KeyE c)
  keyConv                _ = return

  charConv :: Proxy c -> (Window, Char) -> IO (CharE               c)
  default charConv :: (Window, Char) ~ CharE c => Proxy c -> (Window, Char) -> IO (CharE c)
  charConv               _ = return

  charModsConv :: Proxy c -> (Window, Char, ModifierKeys) -> IO (CharModsE c)
  default charModsConv
            :: (Window, Char, ModifierKeys) ~ CharModsE c => Proxy c -> (Window, Char, ModifierKeys) -> IO (CharModsE c)
  charModsConv           _ = return

  mouseButtonConv :: Proxy c -> (Window, MouseButton, MouseButtonState, ModifierKeys) -> IO (MouseButtonE c)
  default mouseButtonConv
            :: (Window, MouseButton, MouseButtonState, ModifierKeys) ~ MouseButtonE c
            => Proxy c -> (Window, MouseButton, MouseButtonState, ModifierKeys) -> IO (MouseButtonE c)
  mouseButtonConv        _ = return

  cursorPosConv :: Proxy c -> (Window, Double, Double) -> IO (CursorPosE c)
  default cursorPosConv
            :: (Window, Double, Double) ~ CursorPosE c => Proxy c -> (Window, Double, Double) -> IO (CursorPosE c)
  cursorPosConv          _ = return

  cursorEnterConv :: Proxy c -> (Window, CursorState) -> IO (CursorEnterE c)
  default cursorEnterConv
            :: (Window, CursorState) ~ CursorEnterE c => Proxy c -> (Window, CursorState) -> IO (CursorEnterE c)
  cursorEnterConv        _ = return

  scrollConv :: Proxy c -> (Window, Double, Double) -> IO (ScrollE c)
  default scrollConv
            :: (Window, Double, Double) ~ ScrollE c => Proxy c -> (Window, Double, Double) -> IO (ScrollE c)
  scrollConv             _ = return

  dropConv :: Proxy c -> (Window, [FilePath]) -> IO (DropE c)
  default dropConv :: (Window, [FilePath]) ~ DropE c => Proxy c -> (Window, [FilePath]) -> IO (DropE c)
  dropConv               _ = return



-- | Type constructor holding default window event types.
data Def

instance Conversion Def where



-- | Wrapped 'GLFW.createWindow' arguments to pass through an event.
data CreateWindow = CreateWindow (Int, Int) String (Maybe Monitor) (Maybe Window)



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
  -> EventSelector t (Action CreateWindow)
  -> GLFWHost ( EventSelector t (Result Window ())
              , Behavior t (Maybe Window)
              , WindowE c t
              )
createWindow' (conv :: Proxy c) hostChan (actionFan :: EventSelector t (Action CreateWindow)) = mdo

  let bearE = select actionFan Create
      killE = select actionFan Destroy

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
    ( fan $ mergeWith (<>) [ flip fmap createdE $ \mayWin ->
                                    case mayWin of
                                      Nothing       -> DMap.singleton Failed mempty
                                      Just (win, _) -> DMap.singleton Created $ pure win
                           , DMap.singleton Destroyed mempty <$ destroyedE
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
