module StateMachine (
    State, accepts, accepts', decide
) where

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
    >>= (\_ -> return $ accepts s xs)

decide :: String -> IO Bool
decide = accepts' S0

