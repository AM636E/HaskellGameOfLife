module GameOfLife (
        gameGrid,
        glider,
        createOpts,
        readGrid,
        nextGeneration,
        RenderFunc,
        GameFunc,
        GameOptions(..),
        Grid
    ) where

import Data.List.Split(splitOn)

type Grid = [[Bool]]
type Coordinate = (Int, Int)
-- Defines a way to render grid.
type RenderFunc = Bool -> Grid -> IO ()
-- A way to run game continiously.
-- Takes GameOptions, Delay, Rendering function.
type GameFunc = GameOptions -> Int -> RenderFunc -> IO ()
data GameOptions = GameOptions {
                      grid :: Grid
                     ,runs :: Int } deriving(Show)


createOpts :: Grid -> Int -> GameOptions
createOpts g r = GameOptions {grid = g, runs = r}


neightbords :: Grid -> Coordinate -> [Coordinate]
neightbords [] _ = []
neightbords xs (x, y) =
    filter
    (\(x', y') ->
        x' > 0 &&
        y' > 0 &&
        x' < w &&
        y' < h)
    [
        (x', y') |
            x' <- [(x - 1) .. (x + 1)],
            y' <- [(y - 1) .. (y + 1)],
            (x', y') /= (x, y)
    ]
    where
        w = length $ head xs
        h = length xs


aliveNeighbords :: Grid -> Coordinate -> [Coordinate]
aliveNeighbords g c = filter (\(x, y) -> ((g!! (y-1) )!! (x-1))) $ neightbords g c

nextGeneration :: Grid -> Grid
nextGeneration g =
    let
        h = length g
        w = length $ head g
        countOfAlive c = length $ aliveNeighbords g c
        item (x, y) = ((g!!(y-1))!!(x-1))
    in
        [
            [
                -- Calculating alive and dead cells.
                -- Death from overpopulation or under population.
                -------------------------------------------
                -- This expression result is False if (x, y) item is True,
                -- And it has less than two or more than three neightbords
                not ( item (x, y) && (countOfAlive (x, y) < 2 ||
                      countOfAlive (x, y) > 3) )
                -------------------------------------------
                    &&
                     (
                        -- Birth
                        -- If (x, y) cell is deal and has three neightbords
                        -- It becomes alive.
                        (not (item (x, y)) && countOfAlive (x, y) == 3) ||
                        (item (x, y) &&
                        -- No change.
                        (countOfAlive (x, y) == 2 || countOfAlive (x, y) == 3))
                     )
                        | x <- [1 .. w]
            ]
                        | y <- [1 .. h]
        ]

gameGrid :: (Int, Int) -> [Coordinate] -> Grid
gameGrid (h, w) cells =
    [
        [
            (x, y) `elem` cells
                    | x <- [1 .. w]
        ]
                    | y <- [1 .. h]
    ]

glider :: Int -> Int -> Grid
glider w h = gameGrid (w, h) [(2,1), (1, 3), (2, 3), (3,3), (3,2)]

readGrid :: FilePath -> IO Grid
readGrid path = do
        contents <- readFile path
        return $ filter (not.null) [ [x == '@' | x <- y] | y <- map cleared (splitOn "\n" contents) ]
        where
            badChars = ['\0', '\160', '\9632', '\NUL', '\r', '\n']
            cleared = filter (`notElem` badChars)
