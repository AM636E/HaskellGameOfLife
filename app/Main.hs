module Main where
import GameOfLife
import GameOfLife.Ui.Gui
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if length args == 2 then do
        board <- readGrid (head args)
        runGameContiniously  ( createOpts board (read (args!!1) :: Int)) (read $ args!!1) renderGui
    else
        runGameContiniously (createOpts (glider 20 20) 10 ) 10 renderGui
