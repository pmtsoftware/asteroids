{-# LANGUAGE Arrows #-}

module Main where

import FRP.Yampa
import Data.Vector2
import SDL  (Renderer, pollEvents)
import Control.Concurrent (threadDelay)
import Data.IORef
import Data.Time.Clock
import System.Random
import Control.Monad (unless)

-- project imports
import Gui
import Commands (getCommands)
-- library imports
import Game

type Position = Vector2 Double
type Velocity = Vector2 Double
type ObjectState = (Position, Velocity)

data Reflection = None | XReflection | YReflection

type GIn = ()
type GOut = [(Position, Velocity)]

main :: IO ()
main = do
  g <- newStdGen
  renderer <- initSDL
  t <- getCurrentTime
  timeRef <- newIORef t
  rh <- reactInit initialize (actuate' renderer) (runMany g) 
  gameLoop timeRef rh
  return ()

gameLoop :: IORef UTCTime ->  ReactHandle GIn GOut -> IO ()
gameLoop t rh = do
  (dt, input) <- sense' t
  flag <- react rh (dt, input)
  commands <- getCommands
  unless (quit commands) (gameLoop t rh)

quit :: [Command] -> Bool
quit = elem Exit

runMany :: RandomGen g => g -> SF GIn GOut
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

sense' :: IORef UTCTime -> IO (Double, Maybe ())
sense' timeRef = do
  now <- getCurrentTime
  lastTime <- readIORef timeRef
  writeIORef timeRef now
  let dt = now `diffUTCTime` lastTime
  return (realToFrac dt, Just ())

initialize :: IO ()
initialize = return ()

actuate' :: Renderer -> ReactHandle GIn GOut -> Bool -> GOut -> IO Bool
actuate' renderer _  _ objects = do
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
