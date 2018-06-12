module Game where

import Menu
import Messages
import System.Exit
import GameState
import GameElements
import LoadSave
import Control.Exception
--import Bot

main = startGame

run = do
    putStrLn startingMessage
    putStrLn startingOptionsHeader
    startGame

menu = startGame

startGame = do
    printMenu startingMenu
    option <- getOption strToStartingMenuOption
    case option of
        NewGame -> startNewGame
        LoadGame -> loadGame
        ExitGame -> exitGame
        WrongValue -> wrongValue startGame

-- Getting choice from user and returning option
getOption strToOption = do
    choice <- getLine
    let option = strToOption choice
    return option

-- Main game loop
startNewGame = do
--    pm <- possibleWolfMoves startingGameState
    gameLoop startingGameState

--TODO: Create this
loadGame = do
    putStrLn "Wczytaj gre"
    savedState <- try (load :: IO GameState) :: IO (Either IOException GameState)
    case savedState of
        Left ex -> do
            putStrLn "Wczytanie nie powiodło się."
            menu
        Right savedState -> do
            --pm <- possibleWolfMoves savedState
            gameLoop savedState
            menu

-- Exit game
exitGame = do
    putStrLn "Pa pa"
    exitSuccess

-- Save game
saveGame gameState = do
    putStrLn "Save"
    save gameState
    --pm <- possibleWolfMoves gameState
    gameLoop gameState

-- Returning error and show starting menu
wrongValue endpoint = do
    printOption WrongValue
    endpoint

-- GameLoop, main game content
-- gameLoop :: GameState -> Wolf -> IO
gameLoop gameState@(GameState wolf sheeps turn)
    | getWinner gameState == WolfWinner = do
      putStrLn loseMessage
      startGame
    | getWinner gameState == SheepWinner = do
      putStrLn winMessage
      startGame
    | turn == WolfTurn  = do
      printGameState gameState
      gameLoop (updateGameStateWolf gameState (Wolf (bestMove gameState)))
    | turn == SheepTurn = do
      printGameState gameState
      chooseInGameOption gameState

-- Showing ang getting in game options
chooseInGameOption gameState = do
    printMenu inGameMenu
    option <- getOption strToInGameMenuOption
    case option of
        NewGame -> startNewGame
        ExitGame -> exitGame
        SaveGame -> saveGame gameState
        MakeMove -> makeMove gameState
        WrongValue -> wrongValue (chooseInGameOption gameState)

-- Getting move option from player and making it
makeMove gameState@(GameState wolf sheeps turn) = do
    putStrLn chooseSheepMessage
    printSheeps sheeps 1
    option <- getOption strToChooseSheepMenuOption
    case option of
        Sheep_1 -> moveSheep (sheeps!!0) gameState
        Sheep_2 -> moveSheep (sheeps!!1) gameState
        Sheep_3 -> moveSheep (sheeps!!2) gameState
        Sheep_4 -> moveSheep (sheeps!!3) gameState
        WrongValue -> wrongValue (makeMove gameState)

-- Moving sheep and run again gameLoop
moveSheep sheep gameState@(GameState wolf sheeps turn) = do
    newSheepPosition <- chooseNewSheepPosition sheep gameState
    gameLoop (updateGameStateSheep gameState (updateSheeps sheeps sheep (Sheep newSheepPosition)))

-- Updating Game State after wolf move
updateGameStateWolf :: GameState -> Wolf -> GameState
updateGameStateWolf (GameState _ ss _) p = GameState p ss SheepTurn

-- Updating Game State after sheep move
updateGameStateSheep :: GameState -> Sheeps -> GameState
updateGameStateSheep (GameState w _ _) ss = GameState w ss WolfTurn

-- Predicting if game is end
getWinner :: GameState -> Winner
getWinner gameState@(GameState wolf@(Wolf(x,y)) sheeps turn)
    | y == 0 = WolfWinner
    | length (possibleWolfMoves gameState) == 0 = SheepWinner
    | otherwise = Neither

-- Getting from user new sheep position
chooseNewSheepPosition sheep gameState = do
    putStrLn chooseNewSheepPositionMessage
    possibleMoves <- possibleSheepMoves sheep gameState
    printPossibleMoves possibleMoves 1
    let listLen = length possibleMoves
    case listLen of
        0 -> do
            putStrLn "Nie możesz wykonać żadnego ruchu tą owcą!"
            makeMove gameState
        1 -> do
            option <- getOption strToChooseNewPositionMenuOption
            case option of
                PossibleMove_1 -> return (possibleMoves!!0)
                PossibleMove_2 -> wrongValue (chooseNewSheepPosition sheep gameState)
                WrongValue -> wrongValue (chooseNewSheepPosition sheep gameState)
        2 -> do
            option <- getOption strToChooseNewPositionMenuOption
            case option of
                PossibleMove_1 -> return (possibleMoves!!0)
                PossibleMove_2 -> return (possibleMoves!!1)
                WrongValue -> wrongValue (chooseNewSheepPosition sheep gameState)


-- Returning possible sheep moves
--possibleSheepMoves :: Sheep -> [Point]
--possibleSheepMoves s@(Sheep point@(x,y)) = do
--    let points = [(x-1,y+1), (x+1,y+1)]
--    let points_in_range = filterOutOfBoard points
--    return points_in_range

possibleWolfMoves :: GameState -> [Point]
possibleWolfMoves gameState@(GameState w@(Wolf point@(x,y)) sheep turn) = filterOccupied inBoardPoints gameState
    where 
    inBoardPoints = filterOutOfBoard points
        where
        points = [(x-1,y-1), (x+1,y-1), (x-1,y+1), (x+1,y+1)]

possibleSheepMoves s@(Sheep point@(x,y)) gameState = do
    let points = [(x-1,y+1), (x+1,y+1)]
    let inBoardPoints = filterOutOfBoard points
    let notOccupiedPoints = filterOccupied inBoardPoints gameState
    return notOccupiedPoints

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

-- Simple "best move" algorithm
bestMove :: GameState -> Point
bestMove s = head (possibleWolfMoves s)

-- AI-induced best move
getBestMove :: GameState -> Point
getBestMove g = head (snd (maximumBy fstCmp rates))
  where
    possible = getPossibleMoves g
    rates = [getGameStateRate m depth | m <- possible]
        where depth = 3

getPossibleMoves :: GameState -> [Point]
getPossibleMoves g@(GameState w hs WolfTurn) = possibleWolfMoves g
getPossibleMoves g@(GameState w hs SheepTurn) = possibleSheepMoves g

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

fstCmp :: (Ord a) => (a,b) -> (a,b) -> Ordering
fstCmp (a1, b1) (a2, b2)
  | a1 > a2 = GT
  | a1 < a2 = LT
  | otherwise = EQ

getGameStateRate :: GameState -> Int -> Int
getGameStateRate g 0 = getGameStateRate' g
getGameStateRate g@(GameState w hs t) d
  | length moves == 0 = getGameStateRate' g
  | t == SheepTurn = minimum rates
  | otherwise = maximum rates
  where
    moves = getPossibleMoves g
    rates = [getGameStateRate m (d-1) | m <- moves]

getGameStateRate' :: GameState -> Int
getGameStateRate' g@(GameState w hs t) =
  case (getWinner g) of
    Sheep  -> 100
    Wolf  -> -100
    Neither -> (getSurroundingSheepCount w hs) * 20 - (wolfDistance w) * 10 - 10 * (fault1 w hs)

getSurroundingSheepCount :: Point -> [Point] -> Int
getSurroundingSheepCount w hs = length [h | h <- hs, isNeighbour w h]

wolfDistance :: Point -> Int
wolfDistance (px, py) = 7 - py

fault1 :: Point -> [Point] -> Int
fault1 _ [] = 0
fault1 w@(_,wy) ((_,hy):hs) = (maximum [0, hy - wy]) + (fault1 w hs)
