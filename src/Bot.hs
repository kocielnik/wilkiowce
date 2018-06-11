module Bot where

import Data.Binary
import System.IO
import Data.Coerce
import GameElements
import GameState

data Position = Position Point deriving (Show, Eq)
--newtype Position a b = Position (a,b)

surroundingFields :: Point -> [Point]
surroundingFields (x,y)
  | x == 0 && y == 0  = [((x+1), (y+1))]
  | x == 0 && y == 7  = [((x+1), (y-1))]
  | x == 7 && y == 0  = [((x-1), (y+1))]
  | x == 7 && y == 7  = [((x-1), (y-1))]
  | x == 0      = [((x+1), (y-1)), ((x+1), (y+1))]
  | x == 7      = [((x-1), (y-1)), ((x-1), (y+1))]
  | y == 0      = [((x-1), (y+1)), ((x+1), (y+1))]
  | y == 7      = [((x-1), (y-1)), ((x+1), (y-1))]
  | otherwise     = [((x-1), (y-1)), ((x-1), (y+1)), ((x+1), (y-1)), ((x+1), (y+1))]

isOccupied :: GameState -> Point -> Bool
isOccupied (GameState w@(Wolf point) [] _) p = point == p
isOccupied g@(GameState w (h@(Sheep point):hs) t) p
  | point == p = True
  | otherwise = isOccupied (GameState w hs t) p
