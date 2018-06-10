module LoadSave where

import Data.Binary
import System.IO
import Data.Coerce
import GameElements
import GameState

data Position = Position Point deriving (Show, Eq)
--newtype Position a b = Position (a,b)

instance Binary Position where
  put (Position (x,y)) = do
    put ((fromIntegral x) :: Word8)
    put ((fromIntegral y) :: Word8)

  get = do
    x <- get :: Get Word8
    y <- get :: Get Word8
    return (Position (((fromIntegral x) :: Int), ((fromIntegral y) :: Int)))

instance Binary Wolf where
  put (Wolf (x,y)) = do
    put ((fromIntegral x) :: Word8)
    put ((fromIntegral y) :: Word8)

  get = do
    x <- get :: Get Word8
    y <- get :: Get Word8
    return (Wolf (((fromIntegral x) :: Int), ((fromIntegral y) :: Int)))

instance Binary Sheep where
  put (Sheep (x,y)) = do
    put ((fromIntegral x) :: Word8)
    put ((fromIntegral y) :: Word8)

  get = do
    x <- get :: Get Word8
    y <- get :: Get Word8
    return (Sheep (((fromIntegral x) :: Int), ((fromIntegral y) :: Int)))

instance Binary Turn where
  put t = case t of
    WolfTurn -> do
      put (0 :: Word8)
    SheepTurn -> do
      put (1 :: Word8)

  get = do
    t <- get :: (Get Word8)
    case t of
      0 -> return WolfTurn
      1 -> return SheepTurn

instance Binary GameState where
  put (GameState w hs t) = do
    put w
    put hs
    put t

  get = do
    w <- get :: Get Wolf
    hs <- get :: Get [Sheep]
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
