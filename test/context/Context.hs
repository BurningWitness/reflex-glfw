{-# LANGUAGE RecordWildCards
           , RecursiveDo
           , TypeFamilies #-}

module Context where

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
  (mainFan, mainB, _windowE) <-
    createWindow
      hostChan
      ( CreateWindow (500, 500) "TestMain" Nothing Nothing <$ postBuildE )
      cascade1E

  let newMainE  = select mainFan CreatedWindow
      cascade0E = leftmost [ () <$ select mainFan DestroyedWindow
                           , select mainFan CouldNotCreateWindow
                           ]

  (auxFan, _auxB) <-
    contextWindow
      hostChan
      ( CreateWindow (32, 32) "Context window" Nothing . Just <$> newMainE )
      cascade2E

  let newAuxE   = select auxFan CreatedWindow
      cascade1E = leftmost [ () <$ select auxFan DestroyedWindow
                           , select auxFan CouldNotCreateWindow
                           ]

  (drawChan, drawChanE) <- newBoundChannel $ leftmost
                                               [ True <$ postBuildE
                                               , False <$ cascade3E
                                               ]

  (contextChan, contextChanE) <- newBoundChannel $ leftmost
                                                     [ True <$ postBuildE
                                                     , False <$ cascade4E
                                                     ]

  let cascade3E = () <$ ffilter not contextChanE
      cascade2E = () <$ ffilter not drawChanE

  postDrawE <- performEventOn drawChan . flip fmap newAuxE $ \w -> do
                                           makeContextCurrent $ Just w
                                           tex <- alloca $ \ptr -> do
                                                    glGenTextures 1 ptr
                                                    peek ptr
                                           glBindTexture GL_TEXTURE_2D tex
                                           glTexParameterf GL_TEXTURE_2D GL_TEXTURE_MIN_LOD 123
                                           print (w, tex)
                                           return tex

  cascade4E <- performEventOn contextChan $ let f (Just w) tex = do
                                                  makeContextCurrent $ Just w
                                                  glBindTexture GL_TEXTURE_2D tex
                                                  res <- alloca $ \ptr -> do
                                                           glGetTexParameterfv GL_TEXTURE_2D GL_TEXTURE_MIN_LOD ptr
                                                           peek ptr
                                                  print (res == 123)
                                                f Nothing  _   = print "No main window?"
                                            in f <$> mainB <@> postDrawE

  return cascade0E



main :: IO ()
main = do
  bracket
    GLFW.init
    (const GLFW.terminate)
    $ \isInit ->
         if not isInit
           then putStrLn "Could not initialize GLFW"
           else hostGLFW network
