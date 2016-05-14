module GameOfLife (
        gameGrid, 
        glider,
        showGrid,
        runGame,
        createOpts,
        readGrid,
        runGameContiniously
    ) where

import Data.List(intercalate, elemIndex)
import Data.List.Split(splitOn)
import System.Console.ANSI
import Control.Concurrent(threadDelay)
import Control.Monad(forM_)

type Grid = [[Bool]]
type Coordinate = (Int, Int)
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
                not ( item (x, y) && (countOfAlive (x, y) < 2 || countOfAlive (x, y) > 3) ) -- Death from overpopulation.
                    &&
                     (
                        (not (item (x, y)) && countOfAlive (x, y) == 3) ||
                        (item (x, y) &&
                        (countOfAlive (x, y) == 2 || countOfAlive (x, y) == 3))
                     ) -- Live continues.
                        | x <- [1 .. w]
            ]
                        | y <- [1 .. h]
        ]

gameGrid :: Coordinate -> [Coordinate] -> Grid
gameGrid (h, w) cells = 
    [
        [
            (x, y) `elem` cells
                    | x <- [1 .. w]
        ] 
                    | y <- [1 .. h]
    ]

showGrid :: Grid -> String
showGrid g =
    let
        w = length (head g)
        h = length g
    in
    intercalate "\n" [
            [
                if (g !! y) !! x then '@' else '-' | x <- [0 .. w - 1]
            ]   | y <- [0 .. h - 1]
        ] ++ "\n"

glider :: Int -> Int -> Grid
glider w h = gameGrid (w, h) [(2,1), (1, 3), (2, 3), (3,3), (3,2)]

runGame :: GameOptions -> IO ()
runGame opts = putStr $ intercalate "\n" $ map showGrid (take (runs opts) $ iterate nextGeneration (grid opts))

-- Redraws only lines with cells .
showGridEffective :: Grid -> IO ()
showGridEffective [] = return ()
showGridEffective g = do
    -- All indexes of lines with True ( cell ).
    let lns = filter (\x -> x /= -1) $ map (\(i,_) ->
                             case i of Just x -> x
                                       Nothing -> -1)  (filter (\(_, x) -> (\t -> t) `any` x) $ map (\x -> (elemIndex x g, x)) g)

    forM_ lns (
        \x -> do
        setCursorPosition x 0
        clearLine
        forM_ (render x) (\ch -> do
                            if ch == '@'
                                then do
                                setSGR [SetColor Foreground Vivid Red]
                                else return ()
                            putChar ch
                            setSGR [Reset]
                            )
        return ())
    where
        render x = [if x' then '@' else ' ' | x' <- (g!!x)]

runGameContiniously :: GameOptions -> Int -> IO ()
runGameContiniously opts delay = do
    let gen = (nextGeneration $ grid opts)
    showGridEffective gen
    threadDelay ((100 * 60) * delay)
    clearFromCursorToScreenBeginning
    runGameContiniously (createOpts gen 10) delay
    return ()

readGrid :: FilePath -> IO Grid
readGrid path = do
        contents <- readFile path
        return [ [x == '@' | x <- y] | y <- (map cleared (splitOn "\n" contents)) ]
        where
            badChars = ['\0', '\160', '\9632', '\NUL', '\r', '\n']
            cleared x = filter (\ch -> not(ch `elem` badChars)) x
