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
    renderGui
) where
import GameOfLife
import Graphics.Gloss
import Data.List.Split(splitOn)
import Data.List(transpose, elemIndices)

renderGui :: RenderFunc
renderGui _ [] = return ()
renderGui True _ = return ()
renderGui False grid = draw grid

window = InWindow "" (700, 700) (10, 10)

clearWindow = color white $ rectangleSolid 700 700
draw grid = display
                window
                white
                (
                  pictures
                    (
                        clearWindow : [
                            renderLine row x |
                                    x <- grid,
                                    row <- [1..(length grid)]
                            ]
                    )
                 )
            where
                renderLine :: Int -> [Bool] -> Picture
                renderLine i l = pictures [color black $
                                           translate (x * 20) (fromIntegral i * 10) $
                                           rectangleSolid 20 20 | x <- trueIndices l ]
                trueIndices = map fromIntegral . elemIndices True
