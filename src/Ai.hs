module Ai where

import Data.List
import Data.Ord
import Prelude
import GameState
import GameElements
import Menu
import MovesPossibilities
import Prelude

-- Creating depth
depth :: Int
depth = 6

-- Returning list of GameStates with new possible positions
getPossibleBoards :: GameState -> [Point] -> [GameState]
getPossibleBoards _ [] = []
getPossibleBoards gameState@(GameState wolf [] SheepTurn) _ = []
getPossibleBoards gameState@(GameState wolf sheeps WolfTurn) (p:pm) = [GameState (Wolf(p)) sheeps SheepTurn] ++ (getPossibleBoards gameState pm)
getPossibleBoards gameState@(GameState wolf (s:ss) SheepTurn) pm = getPossibleSheepBoard (possibleSheepMove s gameState) gameState s ++ (getPossibleBoards (GameState wolf ss SheepTurn) pm)

-- Returning list of GameState with news possible positions for given sheep
getPossibleSheepBoard :: [Point] -> GameState -> Sheep -> [GameState]
getPossibleSheepBoard [] _ _ = []
getPossibleSheepBoard (point:points) gameState@(GameState wolf sheeps _) s = [GameState wolf (updateSheeps sheeps s (Sheep(point))) WolfTurn] ++ (getPossibleSheepBoard points gameState s)

-- Returning best wolf move
getBestMove :: GameState -> GameState
getBestMove g = snd (maximumBy fstCmp rates)
  where
    possible = getPossibleBoards g (possibleWolfMove g)
    rates = [(getGameStateRate m depth, m) | m <- possible]

-- Implementing max-min strategy
getGameStateRate :: GameState -> Int -> Float
getGameStateRate g 0 = getGameStateRate' g
getGameStateRate g@(GameState w hs t) d
  | length moves == 0 = getGameStateRate' g
  | t == SheepTurn = minimum rates
  | otherwise = maximum rates
  where
    pm = possibleWolfMove g
    moves = getPossibleBoards g pm
    rates = [getGameStateRate' g] ++ [getGameStateRate m (d-1) | m <- moves]

-- Creating ordering
fstCmp :: (Ord a) => (a,b) -> (a,b) -> Ordering
fstCmp (a1, b1) (a2, b2)
  | a1 > a2 = GT
  | a1 < a2 = LT
  | otherwise = EQ

-- Returning rate of given GameState
getGameStateRate' :: GameState -> Float
getGameStateRate' g@(GameState w@(Wolf p) ss t) =
  case (getWinner g) of
    SheepWinner -> -100
    WolfWinner -> 100
    Neither -> -10 * fromIntegral (endBoardDistance p)  + 5 * minimum (getDistance ss p) - 10 * fromIntegral (length ((getSurroundingSheeps p ss)))

-- Returning wolf distance from winning position
endBoardDistance :: Point -> Int
endBoardDistance (x,y) = y

-- Returning list of distances from wolf to sheeps
getDistance :: [Sheep] -> Point -> [Float]
getDistance [] _ = []
getDistance (s@(Sheep (sx,sy)):ss) w@(wx,wy) = [sqrt (fromIntegral $ abs(((sx + sx)^2 - (sy + sy)^2)))] ++ getDistance ss w

-- Returning list of surrounding sheeps
getSurroundingSheeps:: Point -> [Sheep] -> [Point]
getSurroundingSheeps _ [] = []
getSurroundingSheeps w (s@(Sheep p):ss) = [p | isNeighbour w p] ++ getSurroundingSheeps w ss

-- Getting if point is near another point.
isNeighbour :: Point -> Point -> Bool
isNeighbour ( px,py) (qx,qy) = (abs (px - qx) == 1) && (abs (py - qy) == 1)