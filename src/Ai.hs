module Ai where

import Data.List
import Data.Ord
import Prelude
import GameState
import GameElements
import Menu
import MovesPossibilities

depth :: Int
depth = 4

getBest :: GameState -> [Int]
getBest g = rates
  where
    pm = possibleWolfMove g
    possible = getPossibleBoards g pm
    rates = [getGameStateRate m depth | m <- possible]

getPossibleBoards :: GameState -> [Point] -> [GameState]
getPossibleBoards _ [] = []
getPossibleBoards gameState@(GameState wolf sheeps turn) (p:pm) = [GameState (Wolf(p)) sheeps WolfTurn] ++ (getPossibleBoards gameState pm)

getBestMove :: GameState -> GameState
getBestMove g = snd (maximumBy fstCmp rates)
  where
    possible = getPossibleBoards g (possibleWolfMove g)
    rates = [(getGameStateRate m depth, m) | m <- possible]

getGameStateRate :: GameState -> Int -> Int
getGameStateRate g 0 = getGameStateRate' g
getGameStateRate g@(GameState w hs t) d
  | length moves == 0 = getGameStateRate' g
  | t == SheepTurn = maximum rates
  | otherwise = maximum rates
  where
    pm = possibleWolfMove g
    moves = getPossibleBoards g pm
    rates = [getGameStateRate m (d-1) | m <- moves]

fstCmp :: (Ord a) => (a,b) -> (a,b) -> Ordering
fstCmp (a1, b1) (a2, b2)
  | a1 > a2 = GT
  | a1 < a2 = LT
  | otherwise = EQ

getGameStateRate' :: GameState -> Int
getGameStateRate' g@(GameState w@(Wolf p) hs t) =
  case (getWinner g) of
    SheepWinner  -> -100
    WolfWinner  -> 100
    Neither -> -10 * endBoardDistance p

endBoardDistance :: Point -> Int
endBoardDistance (x,y) = y

getDistance :: [Sheep] -> Point -> [Float]
getDistance [] _ = []
getDistance (s@(Sheep (sx,sy)):ss) w@(wx,wy) = [sqrt (fromIntegral $ (((sx + sx)^2 - (sy + sy)^2)))] ++ getDistance ss w

fault1 :: Wolf -> [Sheep] -> Int
fault1 _ [] = 0
fault1 w@(Wolf (_,wy)) (h@(Sheep (_,hy)):hs) = (maximum [0, hy - wy]) + (fault1 w hs)