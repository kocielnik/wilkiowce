module Backend where

import Data.Binary
import Data.Ord
import System.IO
import GameElements
import Menu
import GameState

--data Position = Position Int Int deriving (Show, Eq)

--point :: Position -> Point
--point a b = (a, b)

--position :: Point -> Position
--point (a, b) = a b
--type Position a b = (a, b)

data Position a b = Position (a, b) deriving (Show, Eq)

surroundingFields :: Position -> [Position]
surroundingFields (Position x y)
  | x == 0 && y == 0  = [Position (x+1) (y+1)]
  | x == 0 && y == 7  = [Position (x+1) (y-1)]
  | x == 7 && y == 0  = [Position (x-1) (y+1)]
  | x == 7 && y == 7  = [Position (x-1) (y-1)]
  | x == 0      = [(Position (x+1) (y-1)), (Position (x+1) (y+1))]
  | x == 7      = [(Position (x-1) (y-1)), (Position (x-1) (y+1))]
  | y == 0      = [(Position (x-1) (y+1)), (Position (x+1) (y+1))]
  | y == 7      = [(Position (x-1) (y-1)), (Position (x+1) (y-1))]
  | otherwise     = [(Position (x-1) (y-1)), (Position (x-1) (y+1)), (Position (x+1) (y-1)), (Position (x+1) (y+1))]
surroundingFields (Position (x, y)) = surroundingFields (Position x y)

isOccupied :: GameState -> Position -> Bool
isOccupied (GameState w [] _) p = w == p
isOccupied g@(GameState w (h:hs) t) p
  | h == p = True
  | otherwise = isOccupied (GameState w hs t) p

isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

isValidOption :: String -> Int -> Bool
isValidOption s max
  | (isInteger s) = ((read s) <= max && (read s) >= 1)
  | otherwise = False

-- Save

instance Binary Position where
  put (Position x y) = do
    put ((fromIntegral x) :: Word8)
    put ((fromIntegral y) :: Word8)

  get = do
    x <- get :: Get Word8
    y <- get :: Get Word8
    return (Position ((fromIntegral x) :: Int) ((fromIntegral y) :: Int))

instance Binary Turn where
  put t = case t of
    WolfTurn -> do
      put (0 :: Word8)
    SheepTurn -> do
      put (1 :: Word8)

  get = do
    t <- get :: (Get Word8)
    case t of
      0 -> return WolfTurn
      1 -> return SheepTurn

instance Binary GameState where
  put (GameState w hs t) = do
    put w
    put hs
    put t

  get = do
    w <- get :: Get Position
    hs <- get :: Get [Position]
    t <- get :: Get Turn
    return (GameState w hs t)

save :: GameState -> IO ()
save g = do
  putStrLn "Podaj nazwe pliku:"
  putStr "> "
  hFlush stdout
  name <- getLine
  encodeFile name g

load :: IO GameState
load = do
  putStrLn "Podaj nazwe pliku:"
  putStr "> "
  hFlush stdout
  name <- getLine
  decodeFile name :: IO GameState

-- Sheep moves

getSheepMove :: GameState -> GameState -> GameState
getSheepMove (GameState w hs _) (GameState _ newHs _) = (GameState w newHs WolfTurn)

sheepMove :: GameState -> GameState
sheepMove g = getBestMove g

depth :: Int
depth = 4

getBestMove :: GameState -> GameState
getBestMove g = snd (maximumBy fstCmp rates)
  where
    possible = getPossibleMoves g
    rates = [(getGameStateRate m depth, m) | m <- possible]

getPossibleMoves :: GameState -> [GameState]
getPossibleMoves g@(GameState w hs WolfTurn) = [GameState loc hs SheepTurn | loc <- locations]
  where
    locations = [p | p <- (surroundingFields w), not (isOccupied g p)]
getPossibleMoves g@(GameState w hs SheepTurn) = getSheepPossibleMoves g [] hs
--                      done  not done
getSheepPossibleMoves :: GameState -> [Position] -> [Position] -> [GameState]
getSheepPossibleMoves _ _ [] = []
getSheepPossibleMoves g@(GameState w _ _) done (cur@(Position _ hy):ndone) = [(GameState w (done ++ [loc] ++ ndone) WolfTurn) | loc <- locations] ++ (getSheepPossibleMoves g (done ++ [cur]) ndone)
  where locations = [p | p@(Position _ py) <- (surroundingFields cur), (not (isOccupied g p)) && py > hy]

getGameStateRate :: GameState -> Int -> Int
getGameStateRate g 0 = getGameStateRate' g
getGameStateRate g@(GameState w hs t) d
  | length moves == 0 = getGameStateRate' g
  | t == WolfTurn = minimum rates
  | otherwise = maximum rates
  where
    moves = getPossibleMoves g
    rates = [getGameStateRate m (d-1) | m <- moves]

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
