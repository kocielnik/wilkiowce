module Menu where

import Data.List
import GameElements


type Menu = [MenuOption]

data MenuOption = NewGame | LoadGame | ExitGame | SaveGame | MakeMove | WrongValue | Sheep_1 | Sheep_2 | Sheep_3 | Sheep_4 | PossibleMove_1 | PossibleMove_2

-- Creating instance of Show class
instance Show MenuOption where
    show NewGame = "Nowa gra (N)"
    show LoadGame = "Wczytaj grę (L)"
    show ExitGame = "Wyjdź z gry (E)"
    show SaveGame = "Zapisz grę (S)"
    show MakeMove = "Wykonaj ruch (M)"
    show WrongValue = "Podano złą wartość"

-- Converting MenuOption to Output
printOption :: MenuOption -> IO()
printOption option = putStrLn (show option)

-- Returning all Menu options
printMenu :: Menu -> IO()
printMenu [] = putStrLn ""
printMenu (item:menu) = do
    printOption item
    printMenu menu

-- Converting String to MenuOption
strToStartingMenuOption :: String -> MenuOption
strToStartingMenuOption option = case option of
    "N" -> NewGame
    "L" -> LoadGame
    "E" -> ExitGame
    _ -> WrongValue

-- Converting String to MenuOption
strToInGameMenuOption :: String -> MenuOption
strToInGameMenuOption option = case option of
    "N" -> NewGame
    "E" -> ExitGame
    "S" -> SaveGame
    "M" -> MakeMove
    _ -> WrongValue

-- Converting String to MenuOption
strToChooseSheepMenuOption :: String -> MenuOption
strToChooseSheepMenuOption option = case option of
    "1" -> Sheep_1
    "2" -> Sheep_2
    "3" -> Sheep_3
    "4" -> Sheep_4
    _ -> WrongValue

-- Converting String to MenuOption
strToChooseNewPositionMenuOption :: String -> MenuOption
strToChooseNewPositionMenuOption option = case option of
    "1" -> PossibleMove_1
    "2" -> PossibleMove_2
    _ -> WrongValue

-- Creating starting menu
startingMenu :: Menu
startingMenu = [NewGame, LoadGame, ExitGame]

-- Creating in game menu
inGameMenu :: Menu
inGameMenu = [NewGame, ExitGame, SaveGame, MakeMove]

-- Creating choose sheep menu
chooseSheepMenu :: Menu
chooseSheepMenu = [Sheep_1, Sheep_2, Sheep_3, Sheep_4]

-- Creating choose new sheep position menu
chooseSheepPositionMenu :: Menu
chooseSheepPositionMenu = [PossibleMove_1, PossibleMove_2]

-- Creating initial values for possible moves
possibleMove_1 = (0,0)
possibleMove_2 = (0,0)