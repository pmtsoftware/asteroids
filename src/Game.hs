module Game 
  ( GameIn
  , GameOut
  , Command (..)
  ) 
where

import IdentityList
import Data.Vector2

type Position = Vector2 Double
type Velocity = Vector2 Double
type Object = (Position, Velocity)

type GameIn = IL Object
type GameOut = IL Object

type Point = (Int, Int)
type Polygon = [Point]

data Command = Exit | EngineUp | EngineDown | Fire | Left | Right
  deriving (Show, Eq)

--asteroid :: Polygon
--asteroid = [(1,3), (6,0), (13,2), (16,9), (13,15), (6,13), (0,9)]

--test :: IO ()
--test = putStr "jol"
--


