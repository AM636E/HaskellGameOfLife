module Main where
import GameOfLife(runGame, glider)

main :: IO ()
main = runGame 10 $ glider 5 5
