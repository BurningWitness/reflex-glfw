{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Reflex.Host.GLFW as GLFW

import           Control.Concurrent
import           Control.Exception (bracket)
import           Control.Monad.IO.Class
import           Data.GADT.Compare.TH
import           Data.Dependent.Map as DM
import           Data.Functor.Identity
import           Reflex



data Controls a where
  Spawn       :: Controls ()
  BrokenSpawn :: Controls ()
  HostEvent   :: Controls ()
  SecEvent    :: Controls ()
  MoveUp      :: Controls Window
  MoveDown    :: Controls Window
  MoveLeft    :: Controls Window
  MoveRight   :: Controls Window
  Close       :: Controls Window

deriveGEq ''Controls
deriveGCompare ''Controls



-- | This test creates a 500x500 window that should:
--
--     * Spawn a 200x200 window when you press @Enter@ while over it.
--       Each window:
--         
--         * Can be moved in any direction using arrow keys;
--
--         * Can be closed by pressing @Q@;
--
--     * Close itself and all other windows it previously spawned when you press @Q@;
--
--     * Do nothing when you press @B@
--       (it's a check for whether we can spawn the main window twice);
--
--     * Print the thread calling the event and the host thread when pressing
--       either @N@ (called from host thread) or @M@ (redirected onto trigger thread).
network
  :: ( SpiderTimeline Global ~ t
     , SpiderHost Global ~ h
     )
  => Fire t h a
  -> HostChan t
  -> GlobalE t
  -> GLFWHost (Event t ())
network fire hostChan GlobalE {..} = mdo

  liftIO $ putStrLn . ("\nHost thread: " <>) . show =<< myThreadId

  (mainWindowE, _mainWindowB, mainWindowEv) <-
    createWindow
      fire
      ( CreateWindow (500, 500) "TestMain" Nothing Nothing <$ leftmost
                                                                [ postBuildE
                                                                , mainBrokenE
                                                                ]
      )
      $ () <$ mainCloseE

  performEvent_ $ liftIO . putStrLn . ("Main window action: " <>) . show
                    <$> mainWindowE

  let bindings (_, Key'Enter , _, KeyState'Pressed, _) = [Spawn       :=> Identity ()]
      bindings (_, Key'B     , _, KeyState'Pressed, _) = [BrokenSpawn :=> Identity ()]
      bindings (_, Key'N     , _, KeyState'Pressed, _) = [HostEvent   :=> Identity ()]
      bindings (_, Key'M     , _, KeyState'Pressed, _) = [SecEvent    :=> Identity ()]
      bindings (w, Key'Up    , _, KeyState'Pressed, _) = [MoveUp      :=> Identity w ]
      bindings (w, Key'Down  , _, KeyState'Pressed, _) = [MoveDown    :=> Identity w ]
      bindings (w, Key'Left  , _, KeyState'Pressed, _) = [MoveLeft    :=> Identity w ]
      bindings (w, Key'Right , _, KeyState'Pressed, _) = [MoveRight   :=> Identity w ]
      bindings (w, Key'Q     , _, KeyState'Pressed, _) = [Close       :=> Identity w ]
      bindings _                                       = []

      mainControls = fan . fmap DM.fromList $ bindings <$> keyE mainWindowEv

      mainCloseE  = mainControls `select` Close
      mainSpawnE  = mainControls `select` Spawn
      mainBrokenE = mainControls `select` BrokenSpawn
      hostEventE  = mainControls `select` HostEvent
      secEventE   = mainControls `select` SecEvent

  (auxWindowE, _auxWindowB, auxWindowEv) <-
    createWindows
      fire
      (CreateWindow (200, 200) "TestAux" Nothing Nothing <$ mainSpawnE)
      $ leftmost
          [ Just <$> auxCloseE
          , Nothing <$ mainCloseE
          ]

  performEvent_ $ liftIO . putStrLn . ("Auxiliary window action: " <>) . show
                    <$> auxWindowE

  let auxControls = fan . fmap DM.fromList $ foldMap bindings <$> keyE auxWindowEv

      auxCloseE = auxControls `select` Close
      auxUpE    = auxControls `select` MoveUp
      auxDownE  = auxControls `select` MoveDown
      auxLeftE  = auxControls `select` MoveLeft
      auxRightE = auxControls `select` MoveRight

  performEvent_ $ ( \w -> liftIO $ do (x, y) <- GLFW.getWindowPos w
                                      setWindowPos w x (y - 5)
                  ) <$> auxUpE

  performEvent_ $ ( \w -> liftIO $ do (x, y) <- GLFW.getWindowPos w
                                      setWindowPos w x (y + 5)
                  ) <$> auxDownE

  performEvent_ $ ( \w -> liftIO $ do (x, y) <- GLFW.getWindowPos w
                                      setWindowPos w (x - 5) y
                  ) <$> auxLeftE

  performEvent_ $ ( \w -> liftIO $ do (x, y) <- GLFW.getWindowPos w
                                      setWindowPos w (x + 5) y
                  ) <$> auxRightE

  -- Multithreading check

  (hopE, hopRef) <- newHostEvent hostChan

  (redirE, redirRef) <- newTriggerEvent

  performEvent_ $ liftIO (redirRef ()) <$ secEventE

  performEvent_ $ liftIO (hopRef =<< myThreadId) <$ leftmost [ hostEventE, redirE ]

  performEvent_ $ ( \thatThread -> liftIO $ do thisThread <- myThreadId
                                               putStrLn $ mconcat
                                                            [ "Calling from "
                                                            , show thatThread
                                                            , " into "
                                                            , show thisThread
                                                            ]
                  ) <$> hopE

  return $ () <$ mainCloseE



main :: IO ()
main =
  bracket
    GLFW.init
    (const GLFW.terminate)
    $ \isInit ->
         if not isInit
           then putStrLn "Could not initialize GLFW"
           else hostGLFW network
