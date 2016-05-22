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
    showGridEffective
) where
import GameOfLife
import Data.List(intercalate, elemIndex)
import Data.List.Split(splitOn)
import System.Console.ANSI
import Control.Monad(forM_)
import Data.Maybe(fromMaybe)

-- Redraws only lines with cells .
showGridEffective :: RenderFunc
showGridEffective _ [] = return ()
showGridEffective True g = return ()
showGridEffective False g = do
    -- All indexes of lines with True ( cell ).
    let lns = filter (\x -> x /= -1) $
              map (\(i,_) -> fromMaybe (-1) i ) $
              -- All lines with true.
              filter (\(_, x) -> (\t -> t) `any` x) $
              -- List elements with indexes.
              map (\x -> (elemIndex x g, x)) g

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


