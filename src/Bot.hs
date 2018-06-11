module LoadSave where

import Data.Binary
import System.IO
import Data.Coerce
import GameElements
import GameState

data Position = Position Point deriving (Show, Eq)
--newtype Position a b = Position (a,b)

surroundingFields :: Point -> [Point]
surroundingFields (Point x y)
  | x == 0 && y == 0  = [Point (x+1) (y+1)]
  | x == 0 && y == 7  = [Point (x+1) (y-1)]
  | x == 7 && y == 0  = [Point (x-1) (y+1)]
  | x == 7 && y == 7  = [Point (x-1) (y-1)]
  | x == 0      = [(Point (x+1) (y-1)), (Point (x+1) (y+1))]
  | x == 7      = [(Point (x-1) (y-1)), (Point (x-1) (y+1))]
  | y == 0      = [(Point (x-1) (y+1)), (Point (x+1) (y+1))]
  | y == 7      = [(Point (x-1) (y-1)), (Point (x+1) (y-1))]
  | otherwise     = [(Point (x-1) (y-1)), (Point (x-1) (y+1)), (Point (x+1) (y-1)), (Point (x+1) (y+1))]

isOccupied :: GameState -> Point -> Bool
isOccupied (GameState w [] _) p = w == p
isOccupied g@(GameState w (h:hs) t) p
  | h == p = True
  | otherwise = isOccupied (GameState w hs t) p
