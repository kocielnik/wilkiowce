module GameState where

import GameElements
import Data.List


data GameState = GameState Wolf [Sheep] Turn deriving Show

-- Printing GameState Board with line frame
printGameState :: GameState -> IO()
printGameState gameState = do
  let board = gameStateToString gameState
  let framed = boardWithFrame board
  putStrLn ("\n" ++ framed ++ "\n")

-- Converting GameState to String
gameStateToString :: GameState -> String
gameStateToString gameBoard = gameStateToString' gameBoard gameBoard 0 0

gameStateToString' :: GameState -> GameState -> Int -> Int -> String
gameStateToString' _ _ 8 7 = ""
gameStateToString' currentGameBoard baseGameBoard 8 row = "\n" ++ (gameStateToString' baseGameBoard baseGameBoard 0 (row+1))
gameStateToString' currentGameBoard@(GameState w@(Wolf (wx,wy)) ((Sheep (sx,sy)):ss) turn) baseGameBoard col row
  | col == wx && row == wy = "w" ++ next
  | col == sx && row == sy = "o" ++ next
  | otherwise = gameStateToString' (GameState w ss turn) baseGameBoard col row
  where
    next = gameStateToString' baseGameBoard baseGameBoard (col+1) row
gameStateToString' currentGameBoard@(GameState _ [] _) baseGameBoard col row = " " ++ gameStateToString' baseGameBoard baseGameBoard (col+1) row

boardWithFrame :: String -> String
boardWithFrame board = "  12345678 \n +--------+\n" ++ (unlines newRows) ++ " +--------+\n  12345678 "
  where
    rows = indexedList (lines board)
    newRows = [ (show (i+1)) ++ "|" ++ row ++ "|" ++ (show (i+1)) | (i,row) <- rows ]

-- Converting list to indexed list
indexedList :: [a] -> [(Int, a)]
indexedList a = indexedList' 0 a

indexedList' :: Int -> [a] -> [(Int, a)]
indexedList' i (x:xs) = [(i, x)] ++ (indexedList' (i+1) xs)
indexedList' _ [] = []

-- Creating starting GameState
startingGameState :: GameState
startingGameState = GameState (wolf) [(sheep_1), (sheep_2), (sheep_3), (sheep_4)] WolfTurn
