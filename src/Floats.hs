module Floats (
    generateFloats, intf, size, getTwo
) where

generateFloats :: Double -> Double -> Double -> [Double]
generateFloats _ _ 0 = []
generateFloats start end increment
    | start > end = []
    | otherwise = ((end - increment) : (reverse (generateFloats start (end - increment) increment)))

intf :: Double -> Double -> Double
intf x a = (a+1) * (x**a)

size :: [x] -> Int
size [] = 0
size (_:xs) = 1 + (size xs)

getTwo :: IO [Char]
getTwo =
    do
        a <- getChar
        b <- getChar
        return [a, b]

