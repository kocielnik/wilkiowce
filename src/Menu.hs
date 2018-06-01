module Menu where

import Data.List


type Menu = [MenuOption]

data MenuOption = NewGame | LoadGame | ExitGame | WrongValue

-- Creating instance of Show class
instance Show MenuOption where
    show NewGame = "1. Nowa gra"
    show LoadGame = "2. Wczytaj grę"
    show ExitGame = "3. Wyjdź z gry"

-- Converting MenuOption to Output
printOption :: MenuOption -> IO()
printOption option = putStrLn (show option)

-- Returning all Menu options
printMenu :: Menu -> IO()
printMenu [] = putStrLn ""
printMenu (x:xs) = do
    printOption x
    printMenu xs

-- Converting String to MenuOption
strToMenuOption :: String -> MenuOption
strToMenuOption option = case option of
    "1" -> NewGame
    "2" -> LoadGame
    "3" -> ExitGame
    _ -> WrongValue

-- Creating starting menu
startingMenu :: Menu
startingMenu = [NewGame, LoadGame, ExitGame]

