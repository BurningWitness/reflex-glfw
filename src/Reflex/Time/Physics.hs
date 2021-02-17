{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Time.Physics
  ( PhysTick (..)
  , Physrate (..)
  , physrate
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Maybe
import           GHC.Clock
import           Reflex hiding (delay)



data PhysTick =
       PhysTick
         { ptTime   :: Double -- ^ Time when the tick was emitted.
         , ptAnchor :: Double -- ^ Internal reference time. Technically this is the
                              --   time of the first tick, plus all the steps up to
                              --   this point excluding this tick.
         , ptStep   :: Double -- ^ Step to advance physics by.
         }
       deriving Show



data Physrate =
       Physrate
         { pRate      :: Double -- ^ Desired physics engine framerate in frames.
         , pFrameSkip :: Bool   -- ^ In case physics has taken longer than one frame to
                                --   calculate, having this as 'True' will increase the
                                --   step according to the time taken (the step will
                                --   be some multiple of 'pRate'). If 'False', the
                                --   ticks will always advance by 'pRate'.
         }
       deriving Show



phystick :: Maybe PhysTick -> Physrate -> IO PhysTick
phystick mayPrev (Physrate rate hasFrameSkip) = do
  now <- getMonotonicTime
  return $
    case mayPrev of
      Nothing                          -> PhysTick now now rate
      Just (PhysTick _ anchor _) ->
        let inverse = 1 / rate
            new                          -- @n@ is guaranteed to be (>= 1) because the anchor
              | hasFrameSkip             -- always lags behind one frame, (max 1) is for safety
              , now - anchor > inverse = let n = max 1 . floor $ rate * (now - anchor) :: Int
                                         in fromIntegral n / rate
              | otherwise              = inverse
        in PhysTick now (anchor + new) new



-- | @'physrate' configE loopbackE@ fires an 'Event' depending on provided @configE@.
--   The output 'Event' is supposed to feed into the function that calculates physics
--   steps, advancing the world by 'ptStep' and returning @loopbackE@.
--   
--   For @configE@ 'Nothing' represents the output 'Event' never firing and is
--   its default state.
physrate
  :: ( SpiderTimeline Global ~ t
     , SpiderHost Global ~ h
     , MonadHold t m
     , MonadFix m
     , MonadIO (Performable m)
     , TriggerEvent t m
     , PerformEvent t m
     )
  => Event t (Maybe Physrate) -- ^ Configuration
  -> Event t ()               -- ^ Loopback
  -> m (Event t PhysTick)
physrate configE (loopbackE :: Event t ()) = mdo
  
  configB <- hold Nothing configE

  let isActiveB = isJust <$> configB

  resultB <- hold never $ ( \mayRate -> case mayRate of
                                          Nothing -> never
                                          Just _  -> gate isActiveB tickE
                          ) <$> configE

  timeB <- hold Nothing $ leftmost
                            [ Nothing <$ ffilter isNothing configE
                            , Just    <$> gate isActiveB tickE
                            ]

  tickE <- performEventAsync
             $ ( \mayPrev mayRate callback -> do
                    case (mayPrev, mayRate) of
                      (_, Nothing)         -> return ()
                      (Nothing, Just rate) ->
                        liftIO $ do
                          new <- phystick Nothing rate
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
                                new <- phystick mayPrev rate
                                callback new
                            else do
                              new <- phystick mayPrev rate
                              callback new
               ) <$> timeB <@> leftmost
                                 [ configB <@ gate isActiveB loopbackE
                                 , gate (not <$> isActiveB) $ ffilter isJust configE
                                 ]

  return $ switch resultB
