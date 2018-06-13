module MovesPossibilities where

import Data.Binary
import System.IO
import Data.Coerce
import GameElements
import GameState

possibleWolfMove :: GameState -> [Point]
possibleWolfMove gameState@(GameState w@(Wolf point@(x,y)) sheep turn) =
    filterOccupied (filterOutOfBoard [(x-1,y-1), (x+1,y-1), (x-1,y+1), (x+1,y+1)]) gameState

possibleSheepMove :: Sheep -> GameState -> [Point]
possibleSheepMove s@(Sheep point@(x,y)) gameState =
    filterOccupied (filterOutOfBoard [(x-1,y+1), (x+1,y+1)]) gameState

allPossibleSheepMoves :: GameState -> [Sheep] -> [Point]
allPossibleSheepMoves gameState [] = []
allPossibleSheepMoves gameState (s:ss) = possibleSheepMove s gameState ++ allPossibleSheepMoves gameState ss

-- Printing possible moves (2 max)
printPossibleMoves [] _ = putStrLn " "
printPossibleMoves (move@(x,y):moves) n = do
    putStrLn ( "Pole: " ++ "(" ++ (show (x+1)) ++ "," ++ (show (y+1)) ++ ")" ++ " (" ++ (show n) ++ ")")
    printPossibleMoves moves (n+1)

filterOutOfBoard :: [Point] -> [Point]
filterOutOfBoard [] = []
filterOutOfBoard (point:pointList) | onBoard point = [point] ++ filterOutOfBoard pointList
                                   | otherwise = filterOutOfBoard pointList

-- Check if point parameters are inside board coordinates
onBoard :: Point -> Bool
onBoard p@(x,y)
    | x >= 0 && x < 8 && y >= 0 && y < 8 = True
    | otherwise     = False

filterOccupied :: [Point] -> GameState -> [Point]
filterOccupied [] _ = []
filterOccupied (point: pointList) gameState
    | isOccupied gameState point = filterOccupied pointList gameState
    | otherwise = [point] ++ filterOccupied pointList gameState

isOccupied :: GameState -> Point -> Bool
isOccupied (GameState w@(Wolf point) [] _) p = point == p
isOccupied g@(GameState w (h@(Sheep point):hs) t) p
  | point == p = True
  | otherwise = isOccupied (GameState w hs t) p

-- Predicting if game is end
getWinner :: GameState -> Winner
getWinner gameState@(GameState wolf@(Wolf(x,y)) sheeps turn)
    | y == 0 || length (allPossibleSheepMoves gameState sheeps) == 0 = WolfWinner
    | length (possibleWolfMove gameState) == 0 = SheepWinner
    | otherwise = Neither