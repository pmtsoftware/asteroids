{-# LANGUAGE OverloadedStrings #-}

module Gui where

import SDL

initSDL :: IO Renderer
initSDL = do
  initialize [InitVideo]
  window <- createWindow "Asteroids" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer
  present renderer
  return renderer

draw :: (Double, Double) -> Renderer -> IO ()
draw (x, y) renderer = do
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer
  rendererDrawColor renderer $= V4 255 255 255 255
  let xInt = round x
      yInt = round y
  drawRect renderer $ Just (Rectangle (P $ V2 xInt yInt) (V2 20 20))
  present renderer
  return ()

