module Main where
import GameOfLife

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if length args == 2 then do
        board <- readGrid (head args)
        runGameContiniously  ( createOpts board (read (args!!1) :: Int)) (read $ args!!1)
    else
        runGameContiniously (createOpts (glider 20 20) 10 ) 10
