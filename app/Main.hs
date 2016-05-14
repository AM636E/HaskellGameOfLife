module Main where
import GameOfLife

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if length args == 2 then do
        board <- readGrid (head args)
        runGame  ( createOpts board (read (args!!1) :: Int))
    else
        runGame (createOpts (glider 20 20) 10 )
