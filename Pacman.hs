module Pacman where

import Graphics.Gloss

data Game = Game
    { player :: Entity Pacman
    , ghosts :: [Entity Ghost]
    , gold :: [Gold]
    , score :: Int
    , time :: Int
    } deriving (Show, Eq)

data Entity a = Entity
    { xCoord :: Float
    , yCoord :: Float
    , character :: a
    } deriving (Show, Eq)

data Pacman = Pacman deriving (Show, Eq)

data Ghost = Ghost deriving (Show, Eq)

data Gold = Gold deriving (Show, Eq)

run :: IO ()
run = putStrLn "hi"
