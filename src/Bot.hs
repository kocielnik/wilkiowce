module Bot where

import Data.Binary
import System.IO
import Data.Coerce
import GameElements
import GameState

depth :: Int
depth = 3

onBoard :: Point -> Bool
onBoard p@(x,y)
    | x,y <- [1..8] = True
    | otherwise     = False

getBestMove :: GameState -> GameState
getBestMove g = snd (maximumBy fstCmp rates)
  where
    possible = getPossibleMoves g
    rates = [(getGameStateRate m depth, m) | m <- possible]

getPossibleMoves :: GameState -> [GameState]
getPossibleMoves g@(GameState w hs WolfTurn) = [GameState loc hs SheepTurn | loc <- locations]
  where
    locations = [p | p <- (surroundingFields w), not (isOccupied g p)) && py > hy]

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


--possibleMoves :: Wolf -> [Point]
--possibleMoves = do
--    [points] = possibleMoves p
--    return [points@(x, y)) = [surroundingFields p | y < 


isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

isValidOption :: String -> Int -> Bool
isValidOption s max
  | (isInteger s) = ((read s) <= max && (read s) >= 1)
  | otherwise = False

fstCmp :: (Ord a) => (a,b) -> (a,b) -> Ordering
fstCmp (a1, b1) (a2, b2)
  | a1 > a2 = GT
  | a1 < a2 = LT
  | otherwise = EQ

getGameStateRate' :: GameState -> Int
getGameStateRate' g@(GameState w hs t) =
  case (getWinner g) of
    Sheep  -> 100
    Wolf  -> -100
    Neither -> (getSurroundingSheepCount w hs) * 20 - (wolfDistance w) * 10 - 10 * (fault1 w hs)

getSurroundingSheepCount :: Position -> [Position] -> Int
getSurroundingSheepCount w hs = length [h | h <- hs, isNeighbour w h]

isNeighbour :: Position -> Position -> Bool
isNeighbour (Position px py) (Position qx qy) = (abs (px - qx) == 1) && (abs (py - qy) == 1)

wolfDistance :: Position -> Int
wolfDistance (Position px py) = 7 - py

fault1 :: Position -> [Position] -> Int
fault1 _ [] = 0
fault1 w@(Position _ wy) ((Position _ hy):hs) = (maximum [0, hy - wy]) + (fault1 w hs)

getSheepMove :: GameState -> GameState -> GameState
getSheepMove (GameState w hs _) (GameState _ newHs _) = (GameState w newHs WolfTurn)

sheepMove :: GameState -> GameState
sheepMove g = getBestMove g


getGameStateRate :: GameState -> Int -> Int
getGameStateRate g 0 = getGameStateRate' g
getGameStateRate g@(GameState w hs t) d
  | length moves == 0 = getGameStateRate' g
  | t == WolfTurn = minimum rates
  | otherwise = maximum rates
  where
    moves = getPossibleMoves g
    rates = [getGameStateRate m (d-1) | m <- moves]
