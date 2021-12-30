{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Time.Framerate
  ( Tick (..)
  , Framerate (..)
  , framerate
  , FrameId
  ) where

import           Reflex.Channel

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Dependent.Map as DM
import           Data.Dependent.Sum
import           Data.Functor
import           Data.GADT.Compare.TH
import           Data.Maybe
import           Data.Monoid
import           GHC.Clock
import           Graphics.UI.GLFW hiding (getTime)
import           Prelude
import           Reflex hiding (delay)
import           Reflex.Host.Class



newtype FrameId = FrameId Int
                  deriving (Show, Eq, Num)

data Tick =
       Tick
         { tTime   :: Double       -- ^ Time when the tick was emitted.
         , tDelta  :: Maybe Double -- ^ Difference between new and previous 'tTime'.
         , tId     :: FrameId      -- ^ Tick identifier. This is simply the number of
                                   --   meaningful mode flips and it's used to discard old
                                   --   loopbackE occurrences after a switch.
         }
       deriving Show



tick :: Maybe Tick -> FrameId -> IO Tick
tick mayPrevious tickId = do
  tnow <- getMonotonicTime
  let delta = fmap (\prev -> tnow - tTime prev) mayPrevious
  return $ Tick tnow delta tickId



data Framerate = Unlimited     -- ^ No delay between frames.
               | Frames Double -- ^ @(1/n)@s between frames. Lost frames are skipped.
               | VSync Int     -- ^ @n@ screen updates between frames.
               | Asynchronous  -- ^ Frames are only drawn on input 'Event' occurrences,
                               --   with @'VSync' 1@.
                 deriving (Show, Eq)



data FramerateEvent a where
  NowInactive  :: FramerateEvent ()
  NowSync      :: FramerateEvent Framerate
  NowVSync     :: FramerateEvent Int
  NowAsync     :: FramerateEvent ()

deriveGEq ''FramerateEvent
deriveGCompare ''FramerateEvent



-- | @'framerate' configE drawChan asyncE@ fires an 'Event' depending on provided @configE@.
--   The output 'Event' is supposed to feed into the function that draws the game state,
--   swaps the buffers (using 'swapBuffers') and returns @loopbackE@ that passes back the
--   'TickId'.
--
--   For @configE@ 'Nothing' represents the output 'Event' never firing and is
--   its default state.
--
--   During 'Asynchronous' mode @asyncE@ is used to create 'Tick's, firing no more
--   than once per buffer swap and firing additionally once more if @asyncE@ fired
--   after the last tick, but before @loopbackE@ returned.
framerate
  :: ( MonadHold t m
     , MonadFix m
     , MonadIO m
     , MonadIO (Performable m)
     , PerformEvent t m
     , ReflexHost t
     , TriggerEvent t m
     )
  => Event t (Maybe Framerate) -- ^ Configuration
  -> Channel                   -- ^ Drawing thread (for changing 'swapInterval')
  -> Event t ()                -- ^ Asynchronous inputs
  -> Event t FrameId           -- ^ Loopback
  -> m (Event t Tick)
framerate configE drawChan asyncE loopbackE = mdo
  
  configB <- hold Nothing configE

  let isSyncB   = maybe False (/= Asynchronous) <$> configB
      isAsyncB  = maybe False (== Asynchronous) <$> configB

      bindings a =
        case a of
          (Just Unlimited   , Just Unlimited     ) -> []
          (Just (Frames _)  , Just Unlimited     ) -> []
          (Just (VSync _)   , Just Unlimited     ) -> [ NowVSync    ==> 0 ]
          (Just Asynchronous, Just Unlimited     ) -> [ NowSync     ==> Unlimited
                                                      , NowVSync    ==> 0
                                                      ]
          (Nothing          , Just Unlimited     ) -> [ NowSync     ==> Unlimited
                                                      , NowVSync    ==> 0
                                                      ]

          (Just Unlimited   , Just (Frames _)    ) -> []
          (Just (Frames _)  , Just (Frames _)    ) -> []
          (Just (VSync _)   , Just (Frames _)    ) -> [ NowVSync    ==> 0 ]
          (Just Asynchronous, Just (Frames m)    ) -> [ NowSync     ==> Frames m
                                                      , NowVSync    ==> 0
                                                      ]
          (Nothing          , Just (Frames m)    ) -> [ NowSync     ==> Frames m
                                                      , NowVSync    ==> 0
                                                      ]

          (Just Unlimited   , Just (VSync m)     ) -> [ NowVSync    ==> m ]
          (Just (Frames _)  , Just (VSync m)     ) -> [ NowVSync    ==> m ]
          (Just (VSync n)   , Just (VSync m)     ) -> guard (n /= m) $> (NowVSync  ==> m)
          (Just Asynchronous, Just (VSync m)     ) -> mconcat
                                                        [ guard (1 /= m) $> (NowVSync ==> m)
                                                        , pure $ NowSync ==> VSync m
                                                        ]
          (Nothing          , Just (VSync m)     ) -> [ NowSync     ==> VSync m
                                                      , NowVSync    ==> m
                                                      ]

          (Just Unlimited   , Just Asynchronous  ) -> [ NowVSync    ==> 1
                                                      , NowAsync    ==> ()
                                                      ]
          (Just (Frames _)  , Just Asynchronous  ) -> [ NowVSync    ==> 1
                                                      , NowAsync    ==> ()
                                                      ]
          (Just (VSync n)   , Just Asynchronous  ) -> mconcat
                                                        [ guard (n /= 1) $> (NowVSync ==> 1)
                                                        , pure $ NowAsync ==> ()
                                                        ]
          (Just Asynchronous, Just Asynchronous  ) -> []
          (Nothing          , Just Asynchronous  ) -> [ NowVSync    ==> 1
                                                      , NowAsync    ==> ()
                                                      ]

          (Just _           , Nothing            ) -> [ NowInactive ==> () ]

          _                                        -> []

      controls = fan . fmap DM.fromList $ bindings <$> attach configB configE

      nowInactiveE  = controls `select` NowInactive
      nowSyncE      = controls `select` NowSync
      nowVSyncE     = controls `select` NowVSync
      nowAsyncE     = controls `select` NowAsync

  idB <- accumB (\a _ -> a + 1) 0 $ leftmost
                                      [ () <$ nowSyncE
                                      , nowAsyncE
                                      , nowInactiveE
                                      ]

  resultB <- hold never $ leftmost
                            [ tickE <$ nowSyncE
                            , tickE <$ nowAsyncE
                            , never <$ nowInactiveE
                            ]

  timeB <- hold Nothing $ leftmost
                            [ Just <$> tickE
                            , Nothing <$ nowInactiveE
                            ]

  performEventOn_ drawChan $ liftIO . swapInterval <$> nowVSyncE

  -- In asynchronous mode we process only one asyncE Event per draw.
  -- Therefore the first asyncE Event that arrives locks lockedB, and it can later be
  -- unlocked through loopbackE. But if we did drop an asyncE occurrence, we set expectedB,
  -- so once loopbackE arrives it can check that and fire a Tick by itself.
  lockedB <- hold False $ leftmost
                            [ True <$ gate isAsyncB asyncE
                            , False <$ do gate isAsyncB $ gate (not <$> expectedB) loopbackE
                            , False <$ nowSyncE
                            , False <$ nowInactiveE
                            ]

  expectedB <- hold False $ leftmost
                              [ True <$ do gate lockedB $ gate isAsyncB asyncE
                              , False <$ do gate lockedB $ gate isAsyncB loopbackE
                              , False <$ nowSyncE
                              , False <$ nowInactiveE
                              ]


  let unlockedB = not <$> lockedB

  tickE <- performEventAsync
             $ ( \mayPrev tickId (isFresh, rate) callback ->
                   liftIO $ do
                     let deb | isFresh   = tickId + 1
                             | otherwise = tickId
                     case () of
                       () | Just prev <- mayPrev
                          , Frames n <- rate -> do
                              time <- getMonotonicTime
                              -- new time - (previous time + expected delta)
                              -- Flipped signwise to represent the amount of time left until expected time
                              let expected = 1e6 * (1 / n + tTime prev - time)
                              if expected > 0
                                then
                                  void . forkIO $ do
                                    threadDelay $ floor expected
                                    new <- tick mayPrev deb
                                    callback new
                                else do
                                  new <- tick mayPrev deb
                                  callback new

                         | otherwise -> do
                             new <- tick Nothing deb
                             callback new
               ) <$> timeB
                 <*> idB
                 <@> leftmost -- nowSyncE brings the fresh configE, otherwise it's tagMaybe'd on.
                       [ (,) True <$> nowSyncE
                       , (,) False <$> do
                           tagMaybe configB
                             . gate ( (\isSync locked expected -> isSync || (locked && expected))
                                        <$> isSyncB <*> lockedB <*> expectedB
                                    )
                               -- Discards if tick identifiers don't match.
                             . ffilter id $ (==) <$> idB <@> loopbackE
                       , (,) False <$> do
                           tagMaybe configB . gate isAsyncB $ gate unlockedB asyncE
                       ]

  return $ switch resultB
