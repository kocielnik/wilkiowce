module Game where

import Menu
import Messages
import System.Exit


run = do
    putStrLn startingMessage
    putStrLn startingOptionsHeader
    printMenu startingMenu
    option <- getOption strToMenuOption
    case option of
        NewGame -> startNewGame
        LoadGame -> loadGame
        ExitGame -> exitGame

-- getting choice from user and returning option
getOption strToOption = do
    choice <- getLine
    let option = strToOption choice
    return option

--TODO: Create this
startNewGame = do
    putStrLn "Nowa gra"
    run

--TODO: Create this
loadGame = do
    putStrLn "Wczytaj gre"
    run

exitGame = do
    putStrLn "Pa pa"
    exitSuccess