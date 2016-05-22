-----------------------------------------------------------------------------
--
-- Module      :  GameOfLife.Ui.Gui
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

module GameOfLife.Ui.Gui (
    renderGui, runGameGui
) where
import GameOfLife
import Graphics.Gloss
import Data.List.Split(splitOn)
import Data.List(transpose, elemIndices)

renderGui :: RenderFunc
renderGui _ [] = return ()
renderGui True _ = return ()
renderGui False grid = return ()

window = InWindow "" (700, 700) (10, 10)

runGameGui :: GameFunc
runGameGui (GameOptions grid runs) delay renderF =
    play window white delay grid draw (\_ w -> w) (\_ grid -> nextGeneration grid)

rectangle x y w h = color red $ translate x y $ rectangleSolid w h
---
clearWindow = color white $ rectangleSolid 700 700
draw :: Grid -> Picture
draw grid = pictures [
                uncurry renderLine y | y <- indexate grid
            ]

getDiagonal :: [[a]] -> [a]
getDiagonal [] = []
getDiagonal (x:xs) = head x : getDiagonal (map tail xs)

indexate :: [a] -> [(Int, a)]
indexate [] = []
indexate xs = getDiagonal [[(i,x) | x <- xs] | i <- [1..(length xs)]]

renderLine :: Int -> [Bool] -> Picture
renderLine i xs = pictures [
         rectangle (20 *  fromIntegral (fst x))  (fromIntegral i * 20) 20 20 |
         x <- indexate xs, snd x ]

