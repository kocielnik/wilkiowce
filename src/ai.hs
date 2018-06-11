--possibleMoves :: Wolf -> [Point]
--possibleMoves = do
--    [points] = possibleMoves p
--    return [points@(x, y)) = [surroundingFields p | y < 


isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

isValidOption :: String -> Int -> Bool
isValidOption s max
  | (isInteger s) = ((read s) <= max && (read s) >= 1)
  | otherwise = False

fstCmp :: (Ord a) => (a,b) -> (a,b) -> Ordering
fstCmp (a1, b1) (a2, b2)
  | a1 > a2 = GT
  | a1 < a2 = LT
  | otherwise = EQ

getGameStateRate' :: GameState -> Int
getGameStateRate' g@(GameState w hs t) =
  case (getWinner g) of
    Sheep  -> 100
    Wolf  -> -100
    Neither -> (getSurroundingSheepCount w hs) * 20 - (wolfDistance w) * 10 - 10 * (fault1 w hs)

getSurroundingSheepCount :: Position -> [Position] -> Int
getSurroundingSheepCount w hs = length [h | h <- hs, isNeighbour w h]

isNeighbour :: Position -> Position -> Bool
isNeighbour (Position px py) (Position qx qy) = (abs (px - qx) == 1) && (abs (py - qy) == 1)

wolfDistance :: Position -> Int
wolfDistance (Position px py) = 7 - py

fault1 :: Position -> [Position] -> Int
fault1 _ [] = 0
fault1 w@(Position _ wy) ((Position _ hy):hs) = (maximum [0, hy - wy]) + (fault1 w hs)

getSheepMove :: GameState -> GameState -> GameState
getSheepMove (GameState w hs _) (GameState _ newHs _) = (GameState w newHs WolfTurn)

sheepMove :: GameState -> GameState
sheepMove g = getBestMove g


