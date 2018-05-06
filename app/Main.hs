{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Main where

import FRP.Yampa
import FRP.Yampa.Vector2
import SDL  (Renderer)
import Control.Concurrent (threadDelay)
import Data.IORef
import Data.Time.Clock

-- project imports
import Gui

type Position = Vector2 Double
type Velocity = Vector2 Double

main :: IO ()
main = do
  renderer <- initSDL
  t <- getCurrentTime
  timeRef <- newIORef t
  reactimate initialize (sense timeRef) (actuate renderer) (run (vector2 50 50) (vector2 50 50))

sense :: IORef UTCTime -> Bool -> IO (Double, Maybe ())
sense timeRef _ = do
  now <- getCurrentTime
  lastTime <- readIORef timeRef
  writeIORef timeRef now
  let dt = now `diffUTCTime` lastTime
  return (realToFrac dt, Just ())

initialize :: IO ()
initialize = return ()

actuate :: Renderer -> Bool -> (Position, Velocity) -> IO Bool
actuate renderer _ (pos, vel) = do
  let position = vector2XY pos
  draw position renderer
  return False

width :: Double
width = 800

height :: Double
height = 600

minX :: Double
minX = 0

maxX :: Double
maxX = width

minY :: Double
minY = 0

maxY :: Double
maxY = height

runInner :: Position -> Velocity -> SF () (Position, Velocity) 
runInner pos vel = proc input -> do
  pos' <- integral >>^ (^+^ pos) -< vel
  returnA -< (pos', vel)

run :: Position -> Velocity -> SF () (Position, Velocity)
run pos vel = switch first second
  where 
    first = proc input -> do
      (p', v') <- runInner pos vel -< input
      event <- edge -< reflection p'
      returnA -< ((p', vel), event `tag` (p', v'))
    second (pos', vel') = let x = vector2X vel'
                              y = vector2Y vel'
                          in run pos' (vector2 (-x) (-y))
    reflection newPos = let x = vector2X newPos
                            y = vector2Y newPos
                        in x <= minX || x >= maxX || y <= minY || y >= maxY
