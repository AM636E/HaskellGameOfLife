module Main where
import GameOfLife
import GameOfLife.Ui.Gui
import System.Environment

func :: GameFunc
func = runGameGui

main :: IO ()
main = do
    args <- getArgs
    if length args == 2 then do
        board <- readGrid (head args)
        func  ( createOpts board 0) (read $ args!!1) renderGui
    else
        func (createOpts (glider 50 50) 10 ) 10 renderGui
