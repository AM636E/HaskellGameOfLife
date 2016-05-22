-----------------------------------------------------------------------------
--
-- Module      :  GameOfLife.Ui.Text
-- Copyright   :  2016 Author name here
-- License     :  BSD3
--
-- Maintainer  :  bnazariy@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module GameOfLife.Ui.Text (
    showGridEffective, runGameContiniously
) where
import GameOfLife
import Data.List(intercalate, elemIndex)
import Data.List.Split(splitOn)
import System.Console.ANSI
import Control.Monad(forM_, when)
import Data.Maybe(fromMaybe)
import Control.Concurrent(threadDelay)

-- Redraws only lines with cells .
showGridEffective :: RenderFunc
showGridEffective _ [] = return ()
showGridEffective True g = return ()
showGridEffective False g = do
    -- All indexes of lines with True ( cell ).
    let lns = filter (\x -> x /= -1) $
              map (\(i,_) -> fromMaybe (-1) i ) $
              -- All lines with true.
              filter (\(_, x) -> id `any` x) $
              -- List elements with indexes.
              map (\x -> (elemIndex x g, x)) g

    forM_ lns (
        \x -> do
        setCursorPosition x 0
        clearLine
        forM_ (render x) (\ch -> do
                            when (ch == '@') $
                                setSGR [SetColor Foreground Vivid Red]
                            putChar ch
                            setSGR [Reset]
                          )
        return ())
    where
        render x = [if x' then '@' else ' ' | x' <- g !! x]

-- Run game generation after generation.
-- Takes GameOptions delay and render function.
-- Render function takes Grid and bool argument
-- If argument is true than function should clear screen.
-- If false it should draw new generation.
runGameContiniously :: GameFunc
runGameContiniously opts delay renderF = do
    let gen = nextGeneration $ grid opts
    renderF True gen
    renderF False gen
    threadDelay ((100 * 60) * delay)
    runGameContiniously (createOpts gen 10) delay renderF
    return ()

runGame :: GameOptions -> IO ()
runGame opts = putStr $ intercalate "\n" $ map showGrid (take (runs opts) $ iterate nextGeneration (grid opts))

showGrid :: Grid -> String
showGrid [] = ""
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

