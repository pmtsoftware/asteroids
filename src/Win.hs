{-# LANGUAGE OverloadedStrings #-}

module Win where

import SDL.Init
import SDL.Video

init :: IO Window 
init = do
  initialize [InitVideo]
  createWindow "Asteroids" defaultWindow
