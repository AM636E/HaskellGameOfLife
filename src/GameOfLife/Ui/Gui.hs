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
    play window white delay grid draw (\_ w -> w) (\_ grid -> grid)

rectangle x y w h = color red $ translate x y $ rectangleSolid w h

clearWindow = color white $ rectangleSolid 700 700
draw grid = pictures
            (
                clearWindow : [
                renderLine row x |
                           x <- grid,
                           row <- [1..(length grid)]
                 ]
            )
            where
                renderLine :: Int -> [Bool] -> Picture
                renderLine i l = pictures [ rectangle (x * 5) (x * 5) 5 5 |
                                            x <- trueIndices l ]
                trueIndices = map fromIntegral . elemIndices True
