{-# LANGUAGE RecordWildCards
           , RecursiveDo
           , TypeFamilies #-}

module Context where

import           Control.Exception
import           Data.Dependent.Map as DMap
import           Data.Functor (($>))
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
    createWindow hostChan $
      fan $ mergeWith (<>)
              [ postBuildE $> DMap.singleton Create  (pure $ CreateWindow (500, 500) "TestMain" Nothing Nothing)
              , cascade1E  $> DMap.singleton Destroy mempty
              ]

  let newMainE  = select mainFan Created
      cascade0E = leftmost [ select mainFan Destroyed
                           , select mainFan Failed
                           ]

  (auxFan, _auxB) <-
    contextWindow hostChan $
      fan $ mergeWith (<>)
              [ DMap.singleton Create . pure . CreateWindow (32, 32) "Context window" Nothing . Just <$> newMainE
              , cascade2E $> DMap.singleton Destroy mempty
              ]

  let newAuxE   = select auxFan Created
      cascade1E = leftmost [ select auxFan Destroyed
                           , select auxFan Failed
                           ]

  (drawChan, drawFan) <- newBoundChannel . fan $ mergeWith (<>)
                                                   [ postBuildE $> DMap.singleton Create  mempty
                                                   , cascade3E  $> DMap.singleton Destroy mempty
                                                   ]

  (contextChan, contextFan) <- newBoundChannel . fan $ mergeWith (<>)
                                                         [ postBuildE $> DMap.singleton Create  mempty
                                                         , cascade4E  $> DMap.singleton Destroy mempty
                                                         ]
  let cascade3E = leftmost [ select contextFan Destroyed
                           , () <$ select contextFan Failed
                           ]
      cascade2E = leftmost [ select drawFan Destroyed
                           , () <$ select drawFan Failed
                           ]

  postDrawE <- performEventOn drawChan . flip fmap newAuxE $ \w -> do
                                           makeContextCurrent $ Just w
                                           tex <- alloca $ \ptr -> do
                                                    glGenTextures 1 ptr
                                                    peek ptr
                                           glBindTexture GL_TEXTURE_2D tex
                                           glTexParameterf GL_TEXTURE_2D GL_TEXTURE_MIN_LOD 123
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
