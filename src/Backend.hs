
indexedList :: [a] -> [(Int, a)]
indexedList a = indexedList' 0 a

indexedList' :: Int -> [a] -> [(Int, a)]
indexedList' i (x:xs) = [(i, x)] ++ (indexedList' (i+1) xs)
indexedList' _ [] = []

surroundingFields :: Point -> [Point]
surroundingFields (Point x y)
  | x == 0 && y == 0  = [Point (x+1) (y+1)]
  | x == 0 && y == 7  = [Point (x+1) (y-1)]
  | x == 7 && y == 0  = [Point (x-1) (y+1)]
  | x == 7 && y == 7  = [Point (x-1) (y-1)]
  | x == 0      = [(Point (x+1) (y-1)), (Point (x+1) (y+1))]
  | x == 7      = [(Point (x-1) (y-1)), (Point (x-1) (y+1))]
  | y == 0      = [(Point (x-1) (y+1)), (Point (x+1) (y+1))]
  | y == 7      = [(Point (x-1) (y-1)), (Point (x+1) (y-1))]
  | otherwise     = [(Point (x-1) (y-1)), (Point (x-1) (y+1)), (Point (x+1) (y-1)), (Point (x+1) (y+1))]

isOccupied :: GameState -> Point -> Bool
isOccupied (GameState w [] _) p = w == p
isOccupied g@(GameState w (h:hs) t) p
  | h == p = True
  | otherwise = isOccupied (GameState w hs t) p

isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

isValidOption :: String -> Int -> Bool
isValidOption s max
  | (isInteger s) = ((read s) <= max && (read s) >= 1)
  | otherwise = False

-- Save

instance Binary Point where
  put (Point x y) = do
    put ((fromIntegral x) :: Word8)
    put ((fromIntegral y) :: Word8)

  get = do
    x <- get :: Get Word8
    y <- get :: Get Word8
    return (Point ((fromIntegral x) :: Int) ((fromIntegral y) :: Int))

instance Binary Turn where
  put t = case t of
    WolfTurn -> do
      put (0 :: Word8)
    HoundsTurn -> do
      put (1 :: Word8)

  get = do
    t <- get :: (Get Word8)
    case t of
      0 -> return WolfTurn
      1 -> return HoundsTurn

instance Binary GameState where
  put (GameState w hs t) = do
    put w
    put hs
    put t

  get = do
    w <- get :: Get Point
    hs <- get :: Get [Point]
    t <- get :: Get Turn
    return (GameState w hs t)

save :: GameState -> IO ()
save g = do
  putStrLn "Podaj nazwe pliku:"
  putStr "> "
  hFlush stdout
  name <- getLine
  encodeFile name g

load :: IO GameState
load = do
  putStrLn "Podaj nazwe pliku:"
  putStr "> "
  hFlush stdout
  name <- getLine
  decodeFile name :: IO GameState

-- Hounds moves

getHoundsMove :: GameState -> GameState -> GameState
getHoundsMove (GameState w hs _) (GameState _ newHs _) = (GameState w newHs WolfTurn)

houndsMove :: GameState -> GameState
houndsMove g = getBestMove g

depth :: Int
depth = 4

getBestMove :: GameState -> GameState
getBestMove g = snd (maximumBy fstCmp rates)
  where
    possible = getPossibleMoves g
    rates = [(getGameStateRate m depth, m) | m <- possible]

getPossibleMoves :: GameState -> [GameState]
getPossibleMoves g@(GameState w hs WolfTurn) = [GameState loc hs HoundsTurn | loc <- locations]
  where
    locations = [p | p <- (surroundingFields w), not (isOccupied g p)]
getPossibleMoves g@(GameState w hs HoundsTurn) = getHoundPossibleMoves g [] hs
--                      done  not done
getHoundPossibleMoves :: GameState -> [Point] -> [Point] -> [GameState]
getHoundPossibleMoves _ _ [] = []
getHoundPossibleMoves g@(GameState w _ _) done (cur@(Point _ hy):ndone) = [(GameState w (done ++ [loc] ++ ndone) WolfTurn) | loc <- locations] ++ (getHoundPossibleMoves g (done ++ [cur]) ndone)
  where locations = [p | p@(Point _ py) <- (surroundingFields cur), (not (isOccupied g p)) && py > hy]

getGameStateRate :: GameState -> Int -> Int
getGameStateRate g 0 = getGameStateRate' g
getGameStateRate g@(GameState w hs t) d
  | length moves == 0 = getGameStateRate' g
  | t == WolfTurn = minimum rates
  | otherwise = maximum rates
  where
    moves = getPossibleMoves g
    rates = [getGameStateRate m (d-1) | m <- moves]

fstCmp :: (Ord a) => (a,b) -> (a,b) -> Ordering
fstCmp (a1, b1) (a2, b2)
  | a1 > a2 = GT
  | a1 < a2 = LT
  | otherwise = EQ

getGameStateRate' :: GameState -> Int
getGameStateRate' g@(GameState w hs t) =
  case (getWinner g) of
    Hounds  -> 100
    Wolf  -> -100
    Neither -> (getSurroundingHoundsCount w hs) * 20 - (wolfDistance w) * 10 - 10 * (fault1 w hs)

getSurroundingHoundsCount :: Point -> [Point] -> Int
getSurroundingHoundsCount w hs = length [h | h <- hs, isNeighbour w h]

isNeighbour :: Point -> Point -> Bool
isNeighbour (Point px py) (Point qx qy) = (abs (px - qx) == 1) && (abs (py - qy) == 1)

wolfDistance :: Point -> Int
wolfDistance (Point px py) = 7 - py

fault1 :: Point -> [Point] -> Int
fault1 _ [] = 0
fault1 w@(Point _ wy) ((Point _ hy):hs) = (maximum [0, hy - wy]) + (fault1 w hs)
