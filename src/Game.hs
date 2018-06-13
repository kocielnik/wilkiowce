module Game where

import Menu
import Messages
import System.Exit
import GameState
import GameElements
import LoadSave
import Data.List
import Data.Ord
import Prelude
import Control.Exception
import Ai
import MovesPossibilities

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
    gameLoop gameState

-- Returning error and show starting menu
wrongValue endpoint = do
    printOption WrongValue
    endpoint

-- GameLoop, main game content
gameLoop gameState@(GameState wolf sheeps turn)
    | getWinner gameState == WolfWinner = do
      putStrLn loseMessage
      startGame
    | getWinner gameState == SheepWinner = do
      putStrLn winMessage
      startGame
    | turn == WolfTurn  = do
      printGameState gameState
      putStrLn wolfTurnMessage
      let best@(GameState wolf sheeps turn) = getBestMove gameState
      putStr (show (possibleWolfMove gameState))
      putStr (show (getBest gameState))
      gameLoop (updateGameStateWolf gameState wolf)
    | turn == SheepTurn = do
      printGameState gameState
      putStrLn sheepTurnMessage
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

-- Getting from user new sheep position
chooseNewSheepPosition sheep gameState = do
    putStrLn chooseNewSheepPositionMessage
    let possibleMoves = possibleSheepMove sheep gameState
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
