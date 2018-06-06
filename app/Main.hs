{-# LANGUAGE Arrows #-}

module Main where

import FRP.Yampa
import FRP.Yampa.Vector2
import SDL  (Renderer)
import Control.Concurrent (threadDelay)
import Data.IORef
import Data.Time.Clock
import System.Random

-- project imports
import Gui

type Position = Vector2 Double
type Velocity = Vector2 Double
type ObjectState = (Position, Velocity)

data Reflection = None | XReflection | YReflection

main :: IO ()
main = do
  g <- newStdGen
  renderer <- initSDL
  t <- getCurrentTime
  timeRef <- newIORef t
  play g timeRef renderer

play :: RandomGen g => g -> IORef UTCTime -> Renderer -> IO ()
--play g t r = reactimate initialize (sense t) (actuate r) (run p v)
  --where (p, g') = randomPosition g
  --      (v, _)  = randomVelocity g'
play g t r = reactimate initialize (sense t) (actuate r) (runMany g)

runMany :: RandomGen g => g -> SF () [(Position, Velocity)]
runMany g = parB $ take 50 $ objectFactory g

objectFactory :: RandomGen g => g -> [SF () ObjectState]
objectFactory g = fmap cons randomPxyVxy
  where (gP, gV)     = split g
        (gPx, gPy)   = split gP
        (gVx, gVy)   = split gV
        randomPx     = randomRs (0, maxX -10) gPx
        randomPy     = randomRs (0, maxY -10) gPy
        randomPxy    = zip randomPx randomPy
        randomVx     = randomRs (-300, 300) gVx
        randomVy     = randomRs (-300, 300) gVy
        randomVxy    = zip randomVx randomVy
        randomPxyVxy = zip randomPxy randomVxy
        cons ((px, py), (vx, vy)) = let p = vector2 px py
                                        v = vector2 vx vy
                                    in run p v

sense :: IORef UTCTime -> Bool -> IO (Double, Maybe ())
sense timeRef _ = do
  now <- getCurrentTime
  lastTime <- readIORef timeRef
  writeIORef timeRef now
  let dt = now `diffUTCTime` lastTime
  return (realToFrac dt, Just ())

initialize :: IO ()
initialize = return ()

actuate :: Renderer -> Bool -> [(Position, Velocity)] -> IO Bool
actuate renderer _ objects = do
  let positions = fmap (\(p, v) -> vector2XY p) objects
  draw positions renderer
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
run pos vel = dSwitch first second
  where 
    first = proc input -> do
      (p', v') <- runInner pos vel -< input
      event <- arr detectReflection -< (p', v')
      returnA -< ((p', vel), event)
    second (pos', vel') = run pos' vel'

detectReflection :: (Position, Velocity) -> Event (Position, Velocity)
detectReflection (pos, vel) = detect x y vx vy
  where x = vector2X pos
        y = vector2Y pos
        vx = vector2X vel
        vy = vector2Y vel
        detect x' y' vx' vy'
          | vx' < 0 && x' <= minX  =  Event (pos, vector2 (negate vx) vy)
          | vx' > 0 && x' >= maxX  =  Event (pos, vector2 (negate vx) vy)
          | vy' < 0 && y' <= minY  =  Event (pos, vector2 vx (negate vy))
          | vy' > 0 && y' >= maxY  =  Event (pos, vector2 vx (negate vy))
          | otherwise              =  NoEvent
