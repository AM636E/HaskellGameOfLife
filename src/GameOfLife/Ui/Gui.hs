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
---
-----------------------------------------------------------------------------

module GameOfLife.Ui.Gui (
    renderGui, runGameGui
) where
import GameOfLife
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Data.List.Split(splitOn)
import Data.List(transpose, elemIndices)

renderGui :: RenderFunc
renderGui _ [] = return ()
renderGui True _ = return ()
renderGui False grid = return ()

flip' (a, b) = (b, a)
both f (a, b) = (f a, f b)
viewPort = ViewPort (flip' (-700, -500)) (90*2) 0.2

window = InWindow "Game Of Life" (700, 500) (10, 10)

runGameGui :: GameFunc
runGameGui (GameOptions grid runs) delay renderF =
    play window white delay grid draw (\_ w -> w) (\_ grid -> nextGeneration grid)

rectangle x y w h = color red $ translate x y $ rectangleSolid w h

draw :: Grid -> Picture
draw grid = applyViewPortToPicture viewPort $ pictures [
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

