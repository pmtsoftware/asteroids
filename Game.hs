module Game where

import Data.Vector2                                     ( Vector2
                                                        )
import qualified Shapes as Shape
import FRP.Yampa                                        ( constant
                                                        , SF
                                                        )

type I = ()

type O = [Vector2 Float]

runGame :: SF I O
runGame = constant Shape.meteor
