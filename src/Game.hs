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
    pm <- possibleWolfMoves startingGameState
    gameLoop startingGameState pm

--TODO: Create this
loadGame = do
    putStrLn "Wczytaj gre"
    savedState <- try (load :: IO GameState) :: IO (Either IOException GameState)
    case savedState of
        Left ex -> do
            putStrLn "Wczytanie nie powiodło się."
            menu
        Right savedState -> do
            pm <- possibleWolfMoves savedState
            gameLoop savedState pm
            menu

-- Exit game
exitGame = do
    putStrLn "Pa pa"
    exitSuccess

-- Save game
saveGame gameState = do
    putStrLn "Save"
    save gameState
    pm <- possibleWolfMoves gameState
    gameLoop gameState pm

-- Returning error and show starting menu
wrongValue endpoint = do
    printOption WrongValue
    endpoint

-- GameLoop, main game content
gameLoop gameState@(GameState wolf sheeps turn) pm
    | getWinner gameState pm == WolfWinner = do
      putStrLn loseMessage
      startGame
    | getWinner gameState pm == SheepWinner = do
      putStrLn winMessage
      startGame
    | turn == WolfTurn  = do
      printGameState gameState
      gameLoop (updateGameStateWolf gameState (Wolf (2,7))) pm
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
    npm <- possibleWolfMoves (updateGameStateSheep gameState (updateSheeps sheeps sheep (Sheep newSheepPosition)))
    gameLoop (updateGameStateSheep gameState (updateSheeps sheeps sheep (Sheep newSheepPosition))) npm

-- Updating Game State after wolf move
updateGameStateWolf :: GameState -> Wolf -> GameState
updateGameStateWolf (GameState _ ss _) p = GameState p ss SheepTurn

-- Updating Game State after sheep move
updateGameStateSheep :: GameState -> Sheeps -> GameState
updateGameStateSheep (GameState w _ _) ss = GameState w ss WolfTurn

-- Predicting if game is end
getWinner :: GameState -> [Point] -> Winner
getWinner gameState@(GameState wolf@(Wolf(x,y)) sheeps turn) pm
    | y == 0 = WolfWinner
    | length pm == 0 = SheepWinner
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

possibleWolfMoves gameState@(GameState w@(Wolf point@(x,y)) sheep turn) = do
    let points = [(x-1,y-1), (x+1,y-1), (x-1,y+1), (x+1,y+1)]
    let inBoardPoints = filterOutOfBoard points
    let notOccupiedPoints = filterOccupied inBoardPoints gameState
    return notOccupiedPoints

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
