module Main where
import GameOfLife

main :: IO ()
main = runGame (createOpts (glider 20 20) 10 )
