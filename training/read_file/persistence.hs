test_persistence :: IO ()
test_persistence = do
    txt = readFile "save.game"
    
main = test_persistence file_name

-- Data structures

data Point = Point Int Int deriving (Show Eq)
data Winner = Wolf | Hounds | Neither deriving (Show, Eq)
data Turn = WolfTurn | HoundsTurn deriving (Show, Eq)
data GameState = GameState Point [Point] Turn deriving (Show)

data PlayerChoice = ChoiceMove Point | ChoiceExit | ChoiceSave

load :: IO GameState
load = do
    putStrLn "Podaj nazwę pliku:"
    hflush stdout
    name <- getLine
    decodeFile name :: IO GameState

save :: GameState -> IO ()
save g = do
    putstrLn "Podaj nazwę pliku:"
    hFlush stdout
    name <- getLine
    decodeFile name :: IO GameState


