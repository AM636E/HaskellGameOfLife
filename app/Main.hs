module Main where
import  Control.Applicative
import Data.List

data State = S0 | S1 | S2
    deriving(Show)

accepts :: State -> String -> Bool

accepts S0 ('a':xs) = accepts S1 xs
accepts S0 ('b':xs) = accepts S2 xs
accepts S1 ('a':xs) = accepts S2 xs
accepts S1 ('b':xs) = accepts S0 xs
accepts S2 ('a':xs) = accepts S0 xs
accepts S2 ('b':xs) = accepts S2 xs
accepts S2 _        = True
accepts _ _         = False

accepts' :: State -> String -> IO Bool
accepts' s xs = 
    print ((show s) ++ (show xs))
    >>= (\x -> return $ accepts s xs)


decide = accepts' S0

generateFloats :: Double -> Double -> Double -> [Double]
generateFloats _ _ 0 = []
generateFloats start end increment
    | start > end = []
    | otherwise = ((end - increment) : (reverse (generateFloats start (end - increment) increment)))

intf :: Double -> Double -> Double
intf x a = (a+1) * (x**a)

size [] = 0
size (_:xs) = 1 + (size xs)  

getTwo =
    do
        a <- getChar
        b <- getChar
        return [a, b]

main :: IO ()
main = print "Hello"
