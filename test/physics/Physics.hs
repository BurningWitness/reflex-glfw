{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Physics where

import           Reflex.Host.GLFW as GLFW
import           Reflex.Time.Physics

import           Control.Concurrent
import           Control.Exception (bracket)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Functor
import           Data.GADT.Compare.TH
import           Data.Dependent.Map as DMap
import           Data.Dependent.Sum
import           Data.Sequence as S
import           Reflex
import           System.Console.ANSI
import           System.IO



data Controls a where
  On         :: Controls ()
  Off        :: Controls ()
  Faster     :: Controls ()
  Slower     :: Controls ()
  ToggleSkip :: Controls ()
  Close      :: Controls ()

deriveGEq ''Controls
deriveGCompare ''Controls



-- | This test creates a window that should:
--
--     * Start and stop the ticks with @I@ and @O@ respectively. Stopping the ticker
--       will not update the output since no tick is emitted after;
--
--     * Increase the rate by 10 frames when pressing @+@ and decrease by 10 with @-@;
--
--     * Toggle the frame skip using @S@;
--
--     * Close itself when you press @Q@;
--
--   The thread delay on response is set to 10ms, so the framerate __HAS__ to be under 100.
network
  :: ( SpiderTimeline Global ~ t
     , SpiderHost Global ~ h
     )
  => HostChannel
  -> GlobalE t
  -> GLFWHost (Event t ())
network hostChan GlobalE {..} = mdo

  (windowFan, _windowB, WindowE {..}) <-
    createWindow hostChan $
      fan $ mergeWith (<>)
              [ postBuildE $> DMap.singleton Create  (pure $ CreateWindow (500, 500) "TestMain" Nothing Nothing)
              , closeE     $> DMap.singleton Destroy mempty
              ]

  let shutdownE = leftmost
                    [ select windowFan Destroyed
                    , select windowFan Failed
                    ]

      bindings (_, Key'I          , _, KeyState'Pressed, _   ) = pure $ On         ==> ()
      bindings (_, Key'O          , _, KeyState'Pressed, _   ) = pure $ Off        ==> ()
      bindings (_, Key'PadAdd     , _, KeyState'Pressed, _   ) = pure $ Faster     ==> ()
      bindings (_, Key'Equal      , _, KeyState'Pressed, mods) = guard (modifierKeysShift mods)
                                                                   $> do Faster    ==> ()
      bindings (_, Key'PadSubtract, _, KeyState'Pressed, _   ) = pure $ Slower     ==> ()
      bindings (_, Key'Minus      , _, KeyState'Pressed, mods) = guard (modifierKeysShift mods)
                                                                   $> do Slower    ==> ()
      bindings (_, Key'S          , _, KeyState'Pressed, _   ) = pure $ ToggleSkip ==> ()
      bindings (_, Key'Q          , _, KeyState'Pressed, _   ) = pure $ Close      ==> ()
      bindings _                                               = []

      controls = fan . fmap DMap.fromList $ bindings <$> keyE

      onE         = controls `select` On
      offE        = controls `select` Off
      fasterE     = controls `select` Faster
      slowerE     = controls `select` Slower
      toggleSkipE = controls `select` ToggleSkip
      closeE      = controls `select` Close

  let configE = leftmost
                  [ Nothing <$ offE
                  , Just (Physrate 60 False) <$ onE
                  , fmap (\(Physrate rate skip) -> Physrate (rate + 10) skip)
                      <$> configB <@ fasterE
                  , fmap (\(Physrate rate skip) -> Physrate (max 10 $ rate - 10) skip)
                      <$> configB <@ slowerE
                  , fmap (\(Physrate rate skip) -> Physrate rate (not skip) )
                      <$> configB <@ toggleSkipE
                  ]

  configB <- hold Nothing configE

  let frameAcc _      Nothing     = S.empty
      frameAcc frames (Just tick) =
        (ptTime tick, step tick)
          :<| dropWhileR (\(before, _) -> before < ptTime tick - 1) frames

  fpsB <- accumB frameAcc S.empty $ leftmost
                                      [ Nothing <$ offE
                                      , Just <$> loopbackE
                                      ]

  tickE <- physrate configE $ ptId <$> loopbackE

  loopbackE <- performEventAsync $
                 ( \fps mayConfig tick callback ->
                     liftIO .
                       void . forkIO $ do
                         threadDelay 10000
                         putStr $ mconcat
                                    [ saveCursorCode
                                    , clearFromCursorToLineBeginningCode
                                    , "Rate "
                                    , case mayConfig of
                                        Nothing          -> "X"
                                        Just (Physrate rate True) -> show rate <> " with skip"
                                        Just (Physrate rate False) -> show rate <> " no skip"
                                    , " | "
                                    , show $ S.length fps
                                    , "fps, "
                                    , show . sum $ snd <$> fps
                                    , "s"
                                    , restoreCursorCode
                                    ]
                         hFlush stdout
                         callback tick
                 ) <$> fpsB <*> configB <@> tickE

  return shutdownE




main :: IO ()
main = do
  bracket
    GLFW.init
    (const GLFW.terminate)
    $ \isInit ->
         if not isInit
           then putStrLn "Could not initialize GLFW"
           else do
             putStrLn ""
             hostGLFW network
             putStrLn ""
             saveCursor
