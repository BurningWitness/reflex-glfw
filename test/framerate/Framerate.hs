{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Framerate where

import           Reflex.Host.GLFW as GLFW
import           Reflex.Time.Framerate

import           Control.Exception (bracket)
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString as BS
import           Data.Fixed
import           Data.Functor
import           Data.GADT.Compare.TH
import           Data.Dependent.Map as DM
import           Data.Dependent.Sum
import           Data.Sequence as S
import qualified Data.Vector.Storable as VS
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.Rendering.OpenGL hiding (Less)
import           Reflex
import           System.Console.ANSI
import           System.IO



hsb2rgb :: (Ord a, Real a, Floating a) => (a, a, a) -> (a, a, a)
hsb2rgb (h, s, v) =
  let c = s * v
      x = c * (1 - abs ((((h / 60) `mod'` 2) - 1)))
      (r, g, b) | h < 60    = (c, x, 0)
                | h < 120   = (x, c, 0)
                | h < 180   = (0, c, x)
                | h < 240   = (0, x, c)
                | h < 300   = (x, 0, c)
                | otherwise = (c, 0, x)
  in (r, g, b)



initGL :: IO UniformLocation
initGL = do
  vao <- genObjectName
  bindVertexArrayObject $= Just vao

  vbo <- genObjectName
  bindBuffer ArrayBuffer $= Just vbo

  vert <- createShader VertexShader
  vertFile <- BS.readFile "test/framerate/shaders/base130.vert"
  shaderSourceBS vert $= vertFile
  compileShader vert
  isVertCompiled <- get $ compileStatus vert
  unless isVertCompiled $ do
    log' <- get $ shaderInfoLog vert
    fail $ "Could not compile the vertex shader: " <> log'

  frag <- createShader FragmentShader
  fragFile <- BS.readFile "test/framerate/shaders/base130.frag"
  shaderSourceBS frag $= fragFile
  compileShader frag
  isFragCompiled <- get $ compileStatus frag
  unless isFragCompiled $ do
    log' <- get $ shaderInfoLog frag
    fail $ "Could not compile the fragment shader: " <> log'

  prog <- createProgram
  attachShader prog vert
  attachShader prog frag
  linkProgram prog
  isLinked <- linkStatus prog
  unless isLinked $ do
    log' <- get $ programInfoLog prog
    fail $ "Could not link the program: " <> log'

  currentProgram $= Just prog

  let floatSize = sizeOf (undefined :: GLfloat)

  pointLoc <- get (attribLocation prog "point")
  vertexAttribArray pointLoc $= Enabled
  vertexAttribPointer pointLoc $=
    ( ToFloat
    , VertexArrayDescriptor 2 Float (fromIntegral $ 6 * floatSize) nullPtr
    )

  colorLoc <- get (attribLocation prog "color")
  vertexAttribArray colorLoc $= Enabled
  vertexAttribPointer colorLoc $=
    ( ToFloat
    , VertexArrayDescriptor 4 Float (fromIntegral $ 6 * floatSize)
        . intPtrToPtr $ IntPtr (2 * floatSize)
    )

  get (uniformLocation prog "screen")



data Controls a where
  GoOff       :: Controls ()
  GoUnlimited :: Controls ()
  GoFrames    :: Controls ()
  GoVSync     :: Controls ()
  GoAsync     :: Controls ()
  More        :: Controls ()
  Less        :: Controls ()
  Close       :: Controls ()

deriveGEq ''Controls
deriveGCompare ''Controls



-- | This test creates a window that should:
--
--     * Starts the ticks with @U@ (for unlimited framerate), @V@ (for VSync 1),
--       @F@ (for 60fps) or @A@ (for asychronous). Stops the ticks with @O@.
--
--     * Increase the framerate by 10 or VSync by 1 when pressing @+@ and decrease them
--       accordingly in respective modes when pressing @-@.
--
--     * Close itself when you press @Q@;
--
--   The square follows the mouse and slowly changes color.
network
  :: ( SpiderTimeline Global ~ t
     )
  => HostChannel
  -> GlobalE t
  -> GLFWHost (Event t ())
network hostChan GlobalE {..} = mdo

  (windowE, windowB, WindowE {..}) <-
    createWindow
      hostChan
      ( CreateWindow (500, 500) "TestMain" Nothing Nothing <$ postBuildE
      )
      $ closeE

  let bindings (_, Key'O          , _, KeyState'Pressed, _   ) = pure $ GoOff ==> ()
      bindings (_, Key'U          , _, KeyState'Pressed, _   ) = pure $ GoUnlimited ==> ()
      bindings (_, Key'F          , _, KeyState'Pressed, _   ) = pure $ GoFrames ==> ()
      bindings (_, Key'V          , _, KeyState'Pressed, _   ) = pure $ GoVSync ==> ()
      bindings (_, Key'A          , _, KeyState'Pressed, _   ) = pure $ GoAsync ==> ()

      bindings (_, Key'PadAdd     , _, KeyState'Pressed, _   ) = pure $ More ==> ()
      bindings (_, Key'Equal      , _, KeyState'Pressed, mods) = guard (modifierKeysShift mods)
                                                                   $> (More ==> ()) 
      bindings (_, Key'PadSubtract, _, KeyState'Pressed, _   ) = pure $ Less ==> ()
      bindings (_, Key'Minus      , _, KeyState'Pressed, mods) = guard (modifierKeysShift mods)
                                                                   $> (Less ==> ())

      bindings (_, Key'Q          , _, KeyState'Pressed, _   ) = pure $ Close ==> ()
      bindings _                                               = []

      controls = fan . fmap DM.fromList $ bindings <$> keyE

      offE       = controls `select` GoOff
      unlimitedE = controls `select` GoUnlimited
      framesE    = controls `select` GoFrames
      vsyncE     = controls `select` GoVSync
      asyncE     = controls `select` GoAsync
      moreE      = controls `select` More
      lessE      = controls `select` Less
      closeE     = controls `select` Close

  cursorPosB <- hold (-100, -100) $ (\(_, x, y) -> (x, y)) <$> cursorPosE

  (drawChan, drawChanE) <- newBoundChannel $ leftmost
                                               [ True <$ postBuildE
                                               , False <$ closeE
                                               ]

  let configE = leftmost
                  [ Nothing           <$ closeE
                  , Nothing           <$ offE
                  , Just Unlimited    <$ unlimitedE
                  , Just (Frames 60)  <$ framesE
                  , Just (VSync 1)    <$ vsyncE
                  , Just Asynchronous <$ asyncE
                  , fmap ( \case
                              Frames n -> Frames $ n + 10
                              VSync  n -> VSync $ n + 1
                              other    -> other
                         )
                      <$> configB <@ moreE
                  , fmap ( \case
                              Frames n -> Frames . max 10 $ n - 10
                              VSync  n -> VSync . max 1 $ n - 1
                              other    -> other
                         )
                      <$> configB <@ lessE
                  ]

  configB <- hold Nothing configE

  screenLocE <- performEventOn drawChan $
                  ( \window -> do
                      makeContextCurrent $ Just window
                      initGL
                  ) <$> flip fmapMaybe windowE
                          ( \winAction -> case winAction of
                                            CouldNotCreateWindow -> Nothing
                                            CreatedWindow w      -> Just w
                                            DestroyedWindow _    -> Nothing
                          )

  screenLocB <- hold Nothing $ Just <$> screenLocE

  windowSizeB <- hold (500, 500) $ (\(_, w, h) -> (w, h)) <$> windowSizeE

  performEventOn_ drawChan $
    ( \(screenLoc, (_, x, y)) -> do
        uniform screenLoc $= (Vector2 (fromIntegral x) (fromIntegral y) :: Vector2 GLfloat)
        viewport $= (Position 0 0, Size (fromIntegral x) (fromIntegral y))
    ) <$> attachWithMaybe
            (\mayLoc siz -> fmap (\loc -> (loc, siz)) mayLoc)
            screenLocB windowSizeE



  frameE <- framerate configE drawChan (() <$ cursorPosE) loopbackE

  let frameAcc _      Nothing                = S.empty
      frameAcc frames (Just (Tick time _ _)) = 
        time :<| dropWhileR (\before -> before < time - 1) frames

  fpsB <- accumB frameAcc S.empty $ leftmost
                                      [ Nothing <$ asyncE
                                      , Nothing <$ offE
                                      , Just <$> frameE
                                      ]

  loopbackE <- performEventOn drawChan
                 $ ( \mayWindow (x, y) (_, h) fps tick -> do
                       let v = (*6) . realToFrac $ tTime tick `mod'` 60
                           (r, g, b) = hsb2rgb (v, 1, 1 :: GLfloat)

                       clearColor $= Color4 0.1 0.1 0.1 1
                       clear [ColorBuffer]

                       let x' = realToFrac x
                           y' = realToFrac (fromIntegral h - y)

                       let vec = VS.fromList
                                   [ x' + 10, y' + 10, 1, 1, 1, 1 :: GLfloat
                                   , x' - 10, y' + 10, 1, 1, 1, 1
                                   , x' - 10, y' + 10, 1, 1, 1, 1
                                   , x' - 10, y' - 10, 1, 1, 1, 1
                                   , x' - 10, y' - 10, 1, 1, 1, 1
                                   , x' + 10, y' - 10, 1, 1, 1, 1
                                   , x' + 10, y' - 10, 1, 1, 1, 1
                                   , x' + 10, y' + 10, 1, 1, 1, 1
                                   , x' + 15, y' + 15, r, g, b, 1
                                   , x' - 15, y' - 15, r, g, b, 1
                                   , x' - 15, y' + 15, r, g, b, 1
                                   , x' + 15, y' - 15, r, g, b, 1
                                   ]

                       VS.unsafeWith vec $ \ptr -> do
                         let vecSize = fromIntegral $ VS.length vec * sizeOf (undefined :: GLfloat)
                         bufferData ArrayBuffer $= (vecSize, ptr, DynamicDraw)

                       drawArrays Lines 0 12

                       putStr $ mconcat
                                  [ saveCursorCode
                                  , clearFromCursorToLineBeginningCode
                                  , "FPS "
                                  , show $ S.length fps
                                  , " | Time "
                                  , show $ tTime tick
                                  , restoreCursorCode
                                  ]
                       hFlush stdout
                       mapM_ swapBuffers mayWindow

                       return $ tId tick
                   ) <$> windowB <*> cursorPosB <*> windowSizeB <*> fpsB <@> frameE

  -- Crash prevention. We wait for the draw channel to finish before exiting.
  (shutdownE, shutdownRef) <- newTriggerEvent

  performEvent_ $ liftIO . shutdownRef <$> do () <$ ffilter not drawChanE

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
