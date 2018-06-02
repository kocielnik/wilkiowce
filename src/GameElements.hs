module GameElements where

import Data.List


type Point = (Int, Int)

data Turn = WolfTurn | SheepTurn deriving (Show, Eq)

data Wolf = Wolf Point deriving (Show, Eq)

printWolf :: Wolf -> IO()
printWolf wolf = putStrLn (show wolf)

data Sheep = Sheep Point deriving (Show, Eq)

printSheep :: Sheep -> IO()
printSheep sheep = putStrLn (show sheep)

wolf :: Wolf
wolf = Wolf (1,7)

sheep_1 :: Sheep
sheep_1 = Sheep (1,0)

sheep_2 :: Sheep
sheep_2 = Sheep (3,0)

sheep_3 :: Sheep
sheep_3 = Sheep (5,0)

sheep_4 :: Sheep
sheep_4 = Sheep (7,0)