module Main where

import Graphics.Gloss                                   ( Color (..)
                                                        , black, white
                                                        , Picture (..)
                                                        , display
                                                        , Path, Point
                                                        , Display (FullScreen, InWindow)
                                                        )
import Graphics.Gloss.Data.Vector                       ( mulSV
                                                        )
import FRP.Yampa                                        ( Event (..)
                                                        , SF
                                                        , constant
                                                        )
import Data.Vector2                                     ( Vector2
                                                        , vector2X, vector2Y
                                                        )
import Control.Arrow
import Player (playYampa, InputEvent)
import qualified Game as Game

main :: IO ()
main = 
  playYampa
    ( InWindow "Asteroids" (800, 600) (200, 200) )
    black
    20
    game'

game :: IO ()
game = 
  playYampa
    FullScreen
    black
    20
    game'

vectorToPoint :: Vector2 Float -> Point
vectorToPoint v = (vector2X v, vector2Y v)

transform :: Game.O -> Picture
transform = Color white . Line . fmap ( mulSV 20. vectorToPoint )
    
game' :: SF (Event InputEvent) Picture
game' = constant () >>> Game.runGame >>^  transform 
