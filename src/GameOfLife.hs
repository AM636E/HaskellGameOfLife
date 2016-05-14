module GameOfLife (
        gameGrid, 
        glider,
        showGrid,
        runGame
    ) where

import Data.List(intercalate)

type Grid = [[Bool]]
type Coordinate = (Int, Int)

neightbords :: Grid -> Coordinate -> [Coordinate]
neightbords [] _ = []
neightbords xs (x, y) =
    filter
    (\(x', y') -> 
        x' > 0 && 
        y' > 0 && 
        x' < sz && 
        y' < sz)
    [
        (x', y') | 
            x' <- [(x - 1) .. (x + 1)], 
            y' <- [(y - 1) .. (y + 1)],
            (x', y') /= (x, y)
    ]
    where sz = length $ head xs

aliveNeighbords :: Grid -> Coordinate -> [Coordinate]
aliveNeighbords g c = filter (\(x, y) -> ((g!! (y-1) )!! (x-1))) $ neightbords g c

nextGeneration :: Grid -> Grid
nextGeneration grid =
    let
        h = length grid
        w = length $ head grid
        countOfAlive c = length $ aliveNeighbords grid c
        item (x, y) = ((grid!!(y-1))!!(x-1))
    in
        [
            [
                if item (x, y) && (countOfAlive (x, y) < 2 || countOfAlive (x, y) > 3) -- Death from overpopulation.
                    then False
                    else if not (item (x, y)) && countOfAlive (x, y) == 3    -- Born from division.
                        then True
                        else item (x, y) && (countOfAlive (x, y) == 2 || countOfAlive (x, y) == 3) -- Live continues.
                        | x <- [1 .. w]
            ]
                        | y <- [1 .. h]
        ]

gameGrid :: (Int, Int) -> [(Int, Int)] -> Grid
gameGrid (h, w) cells = 
    [
        [
            (x, y) `elem` cells
                    | x <- [1 .. w]
        ] 
                    | y <- [1 .. h]
    ]



showGrid :: Grid -> String
showGrid grid =
    let
        h = length grid
    in
    intercalate "\n" [
            [
                if (grid !! y) !! x then '@' else '-' | x <- [0 .. (h-1)]
            ]   | y <- [0 .. ((length $ head grid) -1 )]
        ] ++ "\n"

glider :: Int -> Int -> Grid
glider w h = gameGrid (w, h) [(2,1), (1, 3), (2, 3), (3,3), (3,2)]

runGame :: Int -> Grid -> IO ()
runGame n grid = putStr $ intercalate "\n" $ map showGrid (take n $ iterate nextGeneration grid)