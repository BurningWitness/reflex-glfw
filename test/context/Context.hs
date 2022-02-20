{-# LANGUAGE RecordWildCards
           , RecursiveDo
           , TypeFamilies #-}

module Context where

import           Control.Concurrent
import           Control.Exception
import           Graphics.GL
import qualified Graphics.UI.GLFW as GLFW
import           Foreign.Marshal.Alloc
import           Foreign.Storable
import           Reflex
import           Reflex.Channel
import           Reflex.Host.GLFW



-- | Checks whether context sharing works. Should print 'True' and exit immediately.
network
  :: ( SpiderTimeline Global ~ t
     )
  => HostChannel
  -> GlobalE t
  -> GLFWHost (Event t ())
network hostChan GlobalE {..} = mdo
  (mainE, mainB, WindowE {..}) <-
    createWindow
      hostChan
      ( CreateWindow (500, 500) "TestMain" Nothing Nothing <$ postBuildE )
      cascadeE1

  let (newMainE, cascadeE0) = fanEither . flip fmap mainE $ \main ->
                                                 case main of
                                                   CreatedWindow w -> Left w
                                                   _               -> Right ()

  (auxE, auxB) <-
    contextWindow
      hostChan
      ( CreateWindow (32, 32) "Context window" Nothing . Just <$> newMainE )
      cascadeE2

  let (newAuxE, cascadeE1) = fanEither . flip fmap auxE $ \aux ->
                                                case aux of
                                                  CreatedWindow w -> Left w
                                                  _               -> Right ()

  (drawChan, drawChanE) <- newBoundChannel $ leftmost
                                               [ True <$ postBuildE
                                               , False <$ cascadeE3
                                               ]

  (contextChan, contextChanE) <- newBoundChannel $ leftmost
                                                     [ True <$ postBuildE
                                                     , False <$ cascadeE4
                                                     ]

  let cascadeE3 = () <$ ffilter not contextChanE
      cascadeE2 = () <$ ffilter not drawChanE

  postDrawE <- performEventOn drawChan . flip fmap newAuxE $ \w -> do
                                           makeContextCurrent $ Just w
                                           tex <- alloca $ \ptr -> do
                                                    glGenTextures 1 ptr
                                                    peek ptr
                                           glBindTexture GL_TEXTURE_2D tex
                                           glTexParameterf GL_TEXTURE_2D GL_TEXTURE_MIN_LOD 123
                                           print (w, tex)
                                           return tex

  cascadeE4 <- performEventOn contextChan $ let f (Just w) tex = do
                                                  makeContextCurrent $ Just w
                                                  glBindTexture GL_TEXTURE_2D tex
                                                  res <- alloca $ \ptr -> do
                                                           glGetTexParameterfv GL_TEXTURE_2D GL_TEXTURE_MIN_LOD ptr
                                                           peek ptr
                                                  print (res == 123)
                                                f Nothing  _   = print "No main window?"
                                            in f <$> mainB <@> postDrawE

  return cascadeE0



main :: IO ()
main = do
  bracket
    GLFW.init
    (const GLFW.terminate)
    $ \isInit ->
         if not isInit
           then putStrLn "Could not initialize GLFW"
           else hostGLFW network
