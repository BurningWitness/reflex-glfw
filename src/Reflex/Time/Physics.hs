{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Time.Physics
  ( PhysTick (..)
  , step
  , Physrate (..)
  , physrate
  , PhysId
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Function
import           Data.Maybe
import           GHC.Clock
import           Reflex hiding (delay)



newtype PhysId = PhysId Int
                 deriving (Show, Eq, Num)

data PhysTick =
       PhysTick
         { ptTime   :: Double -- ^ Time when the tick was emitted.
         , ptAnchor :: Double -- ^ Internal reference time. Technically this is the
                              --   time of the first tick, plus all the steps up to
                              --   this point excluding this tick.
         , ptRate   :: Double -- ^ Current physics engine framerate in frames.
         , ptFrames :: Int    -- ^ Number of frames that happened in between this and the last tick.
                              -- ^ Is guaranteed to be at least 1.
         , ptId     :: PhysId -- ^ Tick identifier. Same reasoning as in "Reflex.Time.Framerate".
         }
       deriving Show

-- | Step the physics engine should take when processing this tick.
step :: PhysTick -> Double
step (PhysTick _ _ rate frames _) = (1 / rate) * fromIntegral frames


data Physrate =
       Physrate
         { pRate      :: Double -- ^ Desired physics engine framerate in frames.
         , pFrameSkip :: Bool   -- ^ In case physics has taken longer than one frame to
                                --   calculate, having this as 'True' will increase the
                                --   step according to the time taken (the step will
                                --   be some multiple of 'pRate'). If 'False', the
                                --   ticks will always advance by 'pRate'.
         }
       deriving (Show, Eq)



phystick :: Maybe PhysTick -> Physrate -> PhysId -> IO PhysTick
phystick mayPrev (Physrate rate hasFrameSkip) tid = do
  tnow <- getMonotonicTime
  return $
    case mayPrev of
      Nothing                        -> PhysTick tnow tnow rate 1 tid
      Just (PhysTick _ anchor _ _ _) ->
        let num
              | hasFrameSkip, tnow - anchor > 1 / rate = floor $ rate * (tnow - anchor)
              | otherwise                              = 1

            res = PhysTick tnow (anchor + step res) rate num tid

        in res



-- | @'physrate' configE loopbackE@ fires an 'Event' depending on provided @configE@.
--   The output 'Event' is supposed to feed into the function that calculates physics
--   steps, advancing the world by 'ptStep' and returning @loopbackE@.
--
--   For @configE@ 'Nothing' represents the output 'Event' never firing and is
--   its default state.
physrate
  :: ( MonadHold t m
     , MonadFix m
     , MonadIO (Performable m)
     , TriggerEvent t m
     , PerformEvent t m
     )
  => Event t (Maybe Physrate) -- ^ Configuration
  -> Event t PhysId           -- ^ Loopback
  -> m (Event t PhysTick)
physrate configE loopbackE = mdo

  configB <- hold Nothing configE

  let isActiveB = isJust <$> configB

  idB <- accumB (\a _ -> a + 1) 0 . ffilter id $ ((/=) `on` isNothing) <$> configB <@> configE

  resultB <- hold never $ ( \mayRate -> case mayRate of
                                          Nothing -> never
                                          Just _  -> gate isActiveB tickE
                          ) <$> configE

  timeB <- hold Nothing $ leftmost
                            [ Nothing <$ ffilter isNothing configE
                            , Just    <$> gate isActiveB tickE
                            ]

  tickE <- performEventAsync
             $ ( \mayPrev tid' (isFresh, mayRate) callback ->
                   let tid | isFresh = tid' + 1
                           | otherwise = tid'
                   in case (mayPrev, mayRate) of
                        (_, Nothing)         -> return ()
                        (Nothing, Just rate) ->
                          liftIO $ do
                            new <- phystick Nothing rate tid
                            callback new
                        (Just prev, Just rate) ->
                          liftIO $ do
                            time <- getMonotonicTime
                            -- new time - (previous time + expected delta)
                            -- Flipped signwise to represent the amount of time left until expected time
                            let expected = 1e6 * (1 / pRate rate + ptAnchor prev - time)
                            if expected > 0
                              then
                                void . forkIO $ do
                                  threadDelay $ truncate expected
                                  new <- phystick mayPrev rate tid
                                  callback new
                              else do
                                new <- phystick mayPrev rate tid
                                callback new
               ) <$> timeB
                 <*> idB
                 <@> leftmost
                       [ (,) False <$> do configB <@ gate isActiveB (ffilter id $ (==) <$> idB <@> loopbackE)
                       , (,) True  <$> do gate (not <$> isActiveB) $ ffilter isJust configE
                       ]

  return $ switch resultB
