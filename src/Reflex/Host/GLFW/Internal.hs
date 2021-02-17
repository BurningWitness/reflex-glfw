{-# OPTIONS_HADDOCK not-home #-}

module Reflex.Host.GLFW.Internal where

import           Control.Concurrent.Chan
import           Control.Concurrent.STM
import           Control.Monad.Ref
import           Data.Bifunctor.Flip
import           Data.Dependent.Sum
import           Reflex
import           Reflex.Host.Class



-- | An additional thread you can initialize and perform 'IO' on.
--
--   The design for this is the idea of keeping a thread alive in between operations
--   and not loading up the trigger thread with things. So, for example, we can just
--   'newBoundChannel', bind the GL context and execute the draw operations sequentially
--   on the thread instead of either making the trigger thread bound and executing
--   operations on it or having to constantly fork around, rebinding the context.
data Channel =
       Channel            -- [Flip (a -> IO ()) :=> IO a]
         { cQueue :: TQueue [DSum (Flip (->) (IO ())) IO]
         , cAlive :: TVar Bool
         }



-- | The host thread itself in the form of a 'Channel'.
newtype HostChannel = HostChannel (TQueue [DSum (Flip (->) (IO ())) IO])



-- | Shorthand.
type GLFWHost = TriggerEventT (SpiderTimeline Global)
                  ( PostBuildT (SpiderTimeline Global)
                      ( PerformEventT (SpiderTimeline Global) (SpiderHost Global)
                      )
                  )



-- | Sets up one single callback. For uses look into 'Reflex.Host.GLFW.Window.hostWindow'.
setup
  :: Chan [DSum (EventTriggerRef t) TriggerInvocation]
  -> Ref IO (Maybe (EventTrigger t c))
  -> (Maybe a -> IO ())
  -> (b -> IO c)
  -> ((b -> IO ()) -> a)
  -> IO ()
setup chan ref callback conv shrink =
  callback . Just . shrink $ \a -> do
    res <- conv a
    writeChan chan [EventTriggerRef ref :=> TriggerInvocation res (return ())]
