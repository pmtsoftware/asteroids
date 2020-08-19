module Shapes 
  ( meteor 
  ) where 

import Data.Vector2                                     ( Vector2
                                                        , vector2
                                                        )

meteor :: [Vector2 Float]
meteor = 
  [ vector2 (-3) (-5)
  , vector2  2 (-3)
  , vector2 4 1
  , vector2 (-1) 4
  , vector2 (-5) (-1)
  , vector2 (-3) (-5)
  ]
