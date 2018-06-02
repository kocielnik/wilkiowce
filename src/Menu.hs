module Menu where

import Data.List
import GameElements


type Menu = [MenuOption]

data MenuOption = NewGame | LoadGame | ExitGame | SaveGame | MakeMove | WrongValue | Sheep_1 | Sheep_2 | Sheep_3 | Sheep_4

-- Creating instance of Show class
instance Show MenuOption where
    show NewGame = "Nowa gra (N)"
    show LoadGame = "Wczytaj grę (L)"
    show ExitGame = "Wyjdź z gry (E)"
    show SaveGame = "Zapisz grę (S)"
    show MakeMove = "Wykonaj ruch (M)"
    show WrongValue = "Podano złą wartość"
    show Sheep_1 = show sheep_1 ++ "(1)"
    show Sheep_2 = show sheep_2 ++ "(2)"
    show Sheep_3 = show sheep_3 ++ "(3)"
    show Sheep_4 = show sheep_4 ++ "(4)"

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
strToMenuOption :: String -> MenuOption
strToMenuOption option = case option of
    "N" -> NewGame
    "L" -> LoadGame
    "E" -> ExitGame
    "S" -> SaveGame
    "M" -> MakeMove
    "1" -> Sheep_1
    "2" -> Sheep_2
    "3" -> Sheep_3
    "4" -> Sheep_4
    _ -> WrongValue

-- Creating starting menu
startingMenu :: Menu
startingMenu = [NewGame, LoadGame, ExitGame]

-- Creating in game menu
inGameMenu :: Menu
inGameMenu = [NewGame, ExitGame, SaveGame, MakeMove]

-- Creating choose sheep menu
chooseSheepMenu = [Sheep_1, Sheep_2, Sheep_3, Sheep_4]
