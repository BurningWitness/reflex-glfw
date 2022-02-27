{-# LANGUAGE DataKinds
           , GADTs
           , KindSignatures #-}

{-# OPTIONS_HADDOCK not-home #-}

module Reflex.Host.GLFW.Internal where

import           Control.Concurrent.Chan
import           Control.Concurrent.STM
import           Control.Monad.Ref
import           Data.Bifunctor.Flip
import           Data.Dependent.Sum
import           Data.GADT.Compare
import           Data.Typeable
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
         , cAlive :: TMVar Bool
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



data Action (t :: *) (a :: *) where
  Create    :: Action t t
  Destroy   :: Action t ()

instance GEq (Action t) where
  Create  `geq` Create  = Just Refl
  Destroy `geq` Destroy = Just Refl
  _       `geq` _       = Nothing

instance GCompare (Action t) where
  Create  `gcompare` Create  = GEQ
  Create  `gcompare` Destroy = GGT
  Destroy `gcompare` Create  = GLT
  Destroy `gcompare` Destroy = GEQ



data Result (t :: *) (e :: *) (a :: *) where
  Created   :: Result t e t
  Destroyed :: Result t e ()
  Failed    :: Result t e e

instance GEq (Result t e) where
  Created   `geq` Created   = Just Refl
  Destroyed `geq` Destroyed = Just Refl
  Failed    `geq` Failed    = Just Refl
  _         `geq` _         = Nothing

instance GCompare (Result t e) where
  Created   `gcompare` Created   = GEQ
  Created   `gcompare` _         = GGT
  _         `gcompare` Created   = GLT

  Destroyed `gcompare` Destroyed = GEQ
  Destroyed `gcompare` _         = GGT
  _         `gcompare` Destroyed = GLT

  Failed    `gcompare` Failed    = GEQ
