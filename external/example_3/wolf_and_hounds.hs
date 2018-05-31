module Main where

import System.IO
import System.Exit
import Data.Binary
import Data.List
import Data.Ord
import Control.Exception

main = menu
-- Menu options
data MainMenuOption = NewGame | Load | Exit | Unknown

menu = do
  printMenu
  choice <- getLine
  let option = translate choice
  case option of
    NewGame -> do
      play newGameState
      menu
    Load -> do
      savedState <- try (load :: IO GameState) :: IO (Either IOException GameState)
      case savedState of
        Left ex -> do
          putStrLn "Nie udało się załadować pliku!"
          menu
        Right gameState -> do
          play gameState
          menu
    Exit -> return ()
    Unknown -> do
      printMenuError
      menu

printMenu = do
  putStrLn "Menu:"
  putStrLn "1. Nowa gra"
  putStrLn "2. Załaduj grę"
  putStrLn "3. Wyjście\n"
  putStr "> "
  hFlush stdout

printMenuError = do
  putStrLn "Nieprawidłowa opcja!\n"

translate :: String -> MainMenuOption
translate "1" = NewGame
translate "2" = Load
translate "3" = Exit
translate _ = Unknown

-- Data model
data Point = Point Int Int deriving (Show, Eq)
data Winner = Wolf | Hounds | Neither deriving (Show, Eq)
data Turn = WolfTurn | HoundsTurn deriving (Show, Eq)
data GameState = GameState Point [Point] Turn deriving (Show)

data PlayerChoice = ChoiceMove Point | ChoiceExit | ChoiceSave

-- Game
newGameState :: GameState
newGameState = GameState (Point 4 7) [(Point 1 0), (Point 3 0), (Point 5 0), (Point 7 0)] WolfTurn

play :: GameState -> IO ()
play g@(GameState w (h:hs) turn)
  | getWinner g == Wolf = do
    printBoard g
    putStrLn "Wygrałeś!\n"
  | getWinner g == Hounds = do
    printBoard g
    putStrLn "Przegrałeś!\n"
  | turn == WolfTurn = do
    printBoard g
    let moves = getWolfPossibleMoves g
    move <- (askPlayerForMove moves)
    case move of
      ChoiceMove p  -> play (applyWolfMove g p)
      ChoiceExit    -> return ()
      ChoiceSave    -> do
        save g
        play g
  | turn == HoundsTurn = do
    let move = (getHoundsMove g (houndsMove g))
    play move

getWinner :: GameState -> Winner
getWinner (GameState (Point _ 0) _ _) = Wolf
getWinner g
  | (length (getWolfPossibleMoves g)) == 0 = Hounds
  | otherwise = Neither

getWolfPossibleMoves :: GameState -> [Point]
getWolfPossibleMoves g@(GameState w _ _) = [p | p <- (surroundingFields w), not (isOccupied g p)]

applyWolfMove :: GameState -> Point -> GameState
applyWolfMove (GameState _ hs _) p = GameState p hs HoundsTurn

-- Printing game board
printBoard :: GameState -> IO ()
printBoard g = do
  let board = getBoardString g
  let framed = boardWithFrame board
  putStrLn ("\n" ++ framed ++ "\n")

getBoardString :: GameState -> String
getBoardString g = getBoardString' g g 0 0

getBoardString' :: GameState -> GameState -> Int -> Int -> String
getBoardString' _ _ 8 7 = ""
getBoardString' g_cur g_org 8 row = "\n" ++ (getBoardString' g_org g_org 0 (row+1))
getBoardString' g_cur@(GameState w@(Point wx wy) ((Point hx hy):hs) turn) g_org col row
  | col == wx && row == wy = "w" ++ next
  | col == hx && row == hy = "o" ++ next
  | otherwise = getBoardString' (GameState w hs turn) g_org col row
  where
    next = getBoardString' g_org g_org (col+1) row
getBoardString' g_cur@(GameState _ [] _) g_org col row = " " ++ getBoardString' g_org g_org (col+1) row

boardWithFrame :: String -> String
boardWithFrame board = "  12345678 \n +--------+\n" ++ (unlines newRows) ++ " +--------+\n  12345678 "
  where
    rows = indexedList (lines board)
    newRows = [ (show (i+1)) ++ "|" ++ row ++ "|" ++ (show (i+1)) | (i,row) <- rows ]

-- Ask player for move
askPlayerForMove :: [Point] -> IO PlayerChoice
askPlayerForMove points = do
  putStrLn "q. Przerwij"
  putStrLn "s. Zapisz stan gry\n"
  putStrLn "Wykonaj ruch:"
  askPlayerForMove' points points 1

askPlayerForMove' :: [Point] -> [Point] -> Int -> IO PlayerChoice
askPlayerForMove' [] p_org _ = do
  putStrLn ""
  putStr "> "
  hFlush stdout
  option <- getLine
  if (isValidOption option (length p_org))
    then do
      let index = (read option) - 1     
      return (ChoiceMove (p_org !! index))
    else case option of
      "q" -> return ChoiceExit
      "s" -> return ChoiceSave
      _ ->  do
            putStrLn "Nieprawidłowy wybór. Spróbuj ponownie:\n"
            askPlayerForMove p_org

askPlayerForMove' ((Point x y):ps) p_org idx = do
  putStrLn ((show idx) ++ ". " ++ (show (x+1)) ++ " " ++ (show (y+1)))
  askPlayerForMove' ps p_org (idx+1)


-- Supporting functions
indexedList :: [a] -> [(Int, a)]
indexedList a = indexedList' 0 a

indexedList' :: Int -> [a] -> [(Int, a)]
indexedList' i (x:xs) = [(i, x)] ++ (indexedList' (i+1) xs)
indexedList' _ [] = []

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

isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

isValidOption :: String -> Int -> Bool
isValidOption s max
  | (isInteger s) = ((read s) <= max && (read s) >= 1)
  | otherwise = False

-- Save

instance Binary Point where
  put (Point x y) = do
    put ((fromIntegral x) :: Word8)
    put ((fromIntegral y) :: Word8)

  get = do
    x <- get :: Get Word8
    y <- get :: Get Word8
    return (Point ((fromIntegral x) :: Int) ((fromIntegral y) :: Int))

instance Binary Turn where
  put t = case t of
    WolfTurn -> do
      put (0 :: Word8)
    HoundsTurn -> do
      put (1 :: Word8)

  get = do
    t <- get :: (Get Word8)
    case t of
      0 -> return WolfTurn
      1 -> return HoundsTurn

instance Binary GameState where
  put (GameState w hs t) = do
    put w
    put hs
    put t

  get = do
    w <- get :: Get Point
    hs <- get :: Get [Point]
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

-- Hounds moves

getHoundsMove :: GameState -> GameState -> GameState
getHoundsMove (GameState w hs _) (GameState _ newHs _) = (GameState w newHs WolfTurn)

houndsMove :: GameState -> GameState
houndsMove g = getBestMove g

depth :: Int
depth = 4

getBestMove :: GameState -> GameState
getBestMove g = snd (maximumBy fstCmp rates)
  where
    possible = getPossibleMoves g
    rates = [(getGameStateRate m depth, m) | m <- possible]

getPossibleMoves :: GameState -> [GameState]
getPossibleMoves g@(GameState w hs WolfTurn) = [GameState loc hs HoundsTurn | loc <- locations]
  where
    locations = [p | p <- (surroundingFields w), not (isOccupied g p)]
getPossibleMoves g@(GameState w hs HoundsTurn) = getHoundPossibleMoves g [] hs
--                      done  not done
getHoundPossibleMoves :: GameState -> [Point] -> [Point] -> [GameState]
getHoundPossibleMoves _ _ [] = []
getHoundPossibleMoves g@(GameState w _ _) done (cur@(Point _ hy):ndone) = [(GameState w (done ++ [loc] ++ ndone) WolfTurn) | loc <- locations] ++ (getHoundPossibleMoves g (done ++ [cur]) ndone)
  where locations = [p | p@(Point _ py) <- (surroundingFields cur), (not (isOccupied g p)) && py > hy]

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
    Hounds  -> 100
    Wolf  -> -100
    Neither -> (getSurroundingHoundsCount w hs) * 20 - (wolfDistance w) * 10 - 10 * (fault1 w hs)

getSurroundingHoundsCount :: Point -> [Point] -> Int
getSurroundingHoundsCount w hs = length [h | h <- hs, isNeighbour w h]

isNeighbour :: Point -> Point -> Bool
isNeighbour (Point px py) (Point qx qy) = (abs (px - qx) == 1) && (abs (py - qy) == 1)

wolfDistance :: Point -> Int
wolfDistance (Point px py) = 7 - py

fault1 :: Point -> [Point] -> Int
fault1 _ [] = 0
fault1 w@(Point _ wy) ((Point _ hy):hs) = (maximum [0, hy - wy]) + (fault1 w hs)
