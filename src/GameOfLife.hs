module GameOfLife (
        gameGrid, 
        nextGeneration, 
        aliveNeighbords,
        showGrid
    ) where

import Data.List(intercalate, iterate)

-- Game of Life
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
    where sz = size $ head xs

aliveNeighbords :: Grid -> Coordinate -> [Coordinate]
aliveNeighbords g c = filter (\(x, y) -> ((g!! (y-1) )!! (x-1)) == True) $ neightbords g c

nextGeneration :: Grid -> Grid
nextGeneration grid =
    let
        h = size grid
        w = size $ head grid
        countOfAlive c = size $ aliveNeighbords grid c
    in
        [
            [
                if countOfAlive (x, y) < 2 || countOfAlive (x, y) >= 3
                    then False
                    else True
                        | x <- [1 .. h]
            ]
                        | y <- [1 .. w]
        ]

gameGrid :: (Int, Int) -> [(Int, Int)] -> Grid
gameGrid (h, w) cells = 
    [
        [
            if (x, y) `elem` cells 
                then True 
                else False 
                    | x <- [1 .. w]
        ] 
                    | y <- [1 .. h]
    ]

size [] = 0;
size (_:xs) = (+1) $ size xs

showGrid :: Grid -> String
showGrid grid =
    let
        h = size grid
    in
    intercalate "\n" [
            [
                if ((grid!!x)!!y) == True
                    then '@'
                        else '-'
                            | x <- [0 .. (h-1)]
            ]
                            | y <- [0 .. ((size $ head grid) -1 )]
        ] ++ "\n"
glider w h = gameGrid (w, h) [(2,1), (1, 3), (2, 3), (3,3), (3,2)]
runGame n grid = putStr $ intercalate "\n" $ map showGrid (take n $ iterate nextGeneration grid)