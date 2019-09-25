module Commands (getCommands) where

import SDL
import Game
import Data.Maybe (mapMaybe)

getCommands :: IO [Command]
getCommands = bindKeysToCommands <$> pollEvents

bindKeysToCommands :: [Event] -> [Command]
bindKeysToCommands = mapMaybe bindKeyToCommand

bindKeyToCommand :: Event -> Maybe Command
bindKeyToCommand e = 
  bind payload
     where payload = eventPayload e
           bind (KeyboardEvent dat) = bind' dat
           bind _ = Nothing
           bind' ed = 
             let 
              isPressed = keyboardEventKeyMotion ed == Pressed
              keysym = keyboardEventKeysym ed
              keycode = keysymKeycode keysym
             in if isPressed 
                   then bindPressed keycode 
                   else bindReleased keycode

bindPressed :: Keycode -> Maybe Command
bindPressed key = 
  case key of 
    KeycodeQ  -> Just Exit
    KeycodeUp -> Just EngineUp
    _         -> Nothing

bindReleased :: Keycode -> Maybe Command
bindReleased key = 
  case key of
    KeycodeUp -> Just EngineDown
    _         -> Nothing
