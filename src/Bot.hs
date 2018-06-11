module Bot where

import Data.Binary
import System.IO
import Data.Coerce
import GameElements
import GameState

data Points = Points Int Int deriving (Eq, Show)

data Position = Position Point deriving (Show, Eq)
--newtype Position a b = Position (a,b)

surroundingFields :: Points -> [Points]
surroundingFields (Points x y)
  | x == 0 && y == 0  = [Points (x+1) (y+1)]
  | x == 0 && y == 7  = [Points (x+1) (y-1)]
  | x == 7 && y == 0  = [Points (x-1) (y+1)]
  | x == 7 && y == 7  = [Points (x-1) (y-1)]
  | x == 0      = [(Points (x+1) (y-1)), (Points (x+1) (y+1))]
  | x == 7      = [(Points (x-1) (y-1)), (Points (x-1) (y+1))]
  | y == 0      = [(Points (x-1) (y+1)), (Points (x+1) (y+1))]
  | y == 7      = [(Points (x-1) (y-1)), (Points (x+1) (y-1))]
  | otherwise     = [(Points (x-1) (y-1)), (Points (x-1) (y+1)), (Points (x+1) (y-1)), (Points (x+1) (y+1))]

isOccupied :: GameState -> Point -> Bool
isOccupied (GameState w@(Wolf point) [] _) p = point == p
isOccupied g@(GameState w (h@(Sheep point):hs) t) p
  | point == p = True
  | otherwise = isOccupied (GameState w hs t) p
