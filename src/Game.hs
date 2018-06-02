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
    printGameState startingGameState
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
gameLoop gameState = do
    wolf <- moveWolf
    let updatedGameState = updateGameStateWolf gameState wolf
    printGameState updatedGameState
    chooseInGameOption updatedGameState

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

-- Returning wolf after position update
moveWolf = do
    -- Tutaj wydaje mi się, że Ai powinno zwracać ruch wilka tzn. obiekt wilk z nowymi wsp.
    return (Wolf (1,3))

-- Return sheep after position update
moveSheep sheep gameState = do
    let sheep = Sheep (1,1)
    printGameState (updateGameStateSheep gameState sheep)

-- Updating Game State after wolf move
updateGameStateWolf :: GameState -> Wolf -> GameState
updateGameStateWolf (GameState _ ss _) p = GameState p ss SheepTurn

-- Updating Game State after sheep move (Alfa)
updateGameStateSheep :: GameState -> Sheep -> GameState
updateGameStateSheep (GameState w _ _) p = GameState w [p] WolfTurn
