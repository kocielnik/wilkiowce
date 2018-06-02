module Game where

import Menu
import Messages
import System.Exit
import GameState
import GameElements


run = do
    putStrLn startingMessage
    putStrLn startingOptionsHeader
    startGame

startGame = do
    printMenu startingMenu
    option <- getOption strToMenuOption
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

--TODO: Create this
startNewGame = do
    gameLoop startingGameState

--TODO: Create this
loadGame = do
    putStrLn "Wczytaj gre"
    startGame

-- Exit game
exitGame = do
    putStrLn "Pa pa"
    exitSuccess

-- Save game
saveGame = do
    putStrLn "Save"
    startGame

-- Returning error and show starting menu
wrongValue endpoint = do
    printOption WrongValue
    endpoint

-- GameLoop, main game content
gameLoop gameState@(GameState wolf sheeps turn)
    | turn == WolfTurn  = do
      printGameState gameState
      -- Tutaj zamiast Wolf (1,3) będzie wynik predykcji AI
      gameLoop (updateGameStateWolf gameState (Wolf (1,3)))
    | turn == SheepTurn = do
      printGameState gameState
      chooseInGameOption gameState

-- Showing ang getting in game options
chooseInGameOption gameState = do
    printMenu inGameMenu
    option <- getOption strToMenuOption
    case option of
        NewGame -> startNewGame
        ExitGame -> exitGame
        SaveGame -> saveGame
        MakeMove -> makeMove gameState
        WrongValue -> wrongValue (chooseInGameOption gameState)

-- Getting move option from player and making it
makeMove gameState = do
    printMenu chooseSheepMenu
    option <- getOption strToMenuOption
    case option of
        Sheep_1 -> moveSheep sheep_1 gameState
        Sheep_2 -> moveSheep sheep_2 gameState
        Sheep_3 -> moveSheep sheep_3 gameState
        Sheep_4 -> moveSheep sheep_4 gameState
        WrongValue -> wrongValue (makeMove gameState)

-- Returning sheep after position update
moveSheep sheep gameState@(GameState wolf sheeps turn) = do
    -- Tu zamiast Sheep (1,1) będzie wynik wyboru użytkownika z możliwch opcji
    gameLoop (updateGameStateSheep gameState (updateSheeps sheeps sheep (Sheep (1,1))))

-- Updating Game State after wolf move
updateGameStateWolf :: GameState -> Wolf -> GameState
updateGameStateWolf (GameState _ ss _) p = GameState p ss SheepTurn

-- Updating Game State after sheep move (Alfa)
updateGameStateSheep :: GameState -> Sheeps -> GameState
updateGameStateSheep (GameState w _ _) ss = GameState w ss WolfTurn
