import System.IO



main = do
    handle <- openFile "game.save" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
