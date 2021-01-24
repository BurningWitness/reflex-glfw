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
import           Control.Monad.Fix
import           Data.GADT.Compare.TH
import           Data.Dependent.Map as DM
import           Data.Functor.Identity
import           Reflex



data Controls a where
  Spawn       :: Controls ()
  BrokenSpawn :: Controls ()
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
--       (it's a check for whether we can spawn the main window twice).
network
  :: ( SpiderTimeline Global ~ t
     , SpiderHost Global ~ h
     , MonadHold t m
     , MonadIO m
     , MonadIO (Performable m)
     , MonadFix m
     , PerformEvent t m
     , Reflex t
     , TriggerEvent t m
     )
  => Fire t h a
  -> GlobalE t
  -> m (Event t ())

network fire GlobalE {..} = mdo

  (mainWindowD, mainWindowE) <-
    createWindow
      fire
      ( CreateWindow (500, 500) "TestMain" Nothing Nothing <$ leftmost
                                                                [ postBuildE
                                                                , mainBrokenE
                                                                ]
      )
      $ () <$ mainCloseE

  performEvent_ $ liftIO . print . ("Main window action: " <>) . show
                    <$> updated mainWindowD

  let bindings (_, Key'Enter , _, KeyState'Pressed, _) = [Spawn       :=> Identity ()]
      bindings (_, Key'B     , _, KeyState'Pressed, _) = [BrokenSpawn :=> Identity ()]
      bindings (w, Key'Up    , _, KeyState'Pressed, _) = [MoveUp      :=> Identity w ]
      bindings (w, Key'Down  , _, KeyState'Pressed, _) = [MoveDown    :=> Identity w ]
      bindings (w, Key'Left  , _, KeyState'Pressed, _) = [MoveLeft    :=> Identity w ]
      bindings (w, Key'Right , _, KeyState'Pressed, _) = [MoveRight   :=> Identity w ]
      bindings (w, Key'Q     , _, KeyState'Pressed, _) = [Close       :=> Identity w ]
      bindings _                                       = []

      mainControls = fan . fmap DM.fromList $ bindings <$> keyE mainWindowE

      mainCloseE  = mainControls `select` Close
      mainSpawnE  = mainControls `select` Spawn
      mainBrokenE = mainControls `select` BrokenSpawn

  (auxWindowF, _auxWindowB, auxWindowE) <-
    createWindows
      fire
      (CreateWindow (200, 200) "TestAux" Nothing Nothing <$ mainSpawnE)
      $ leftmost
          [ Just <$> auxCloseE
          , Nothing <$ mainCloseE
          ]

  performEvent_ $ liftIO . print . ("Auxiliary window action: " <>) . show
                    <$> auxWindowF

  let auxControls = fan . fmap DM.fromList $ foldMap bindings <$> keyE auxWindowE

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
