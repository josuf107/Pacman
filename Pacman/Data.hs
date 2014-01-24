module Pacman.Data where

import Data.Lens.Common

data Game = Game
    { _player :: Entity Pacman
    , _ghosts :: [Entity Ghost]
    , _gold :: [Gold]
    , _score :: Int
    , _time :: Int
    } deriving (Show, Eq)

player :: Lens Game (Entity Pacman)
player = lens _player (\p g -> g { _player = p})

ghosts :: Lens Game [Entity Ghost]
ghosts = lens _ghosts (\gs g -> g { _ghosts = gs})

gold :: Lens Game [Gold]
gold = lens _gold (\gld g -> g { _gold = gld})

score :: Lens Game Int
score = lens _score (\s g -> g { _score = s})

time :: Lens Game Int
time = lens _time (\t g -> g { _time = t})

data Entity a = Entity
    { _xCoord :: Float
    , _yCoord :: Float
    , _character :: a
    } deriving (Show, Eq)

xCoord :: Lens (Entity a) Float
xCoord = lens _xCoord (\x g -> g { _xCoord = x})

yCoord :: Lens (Entity a) Float
yCoord = lens _yCoord (\y g -> g { _yCoord = y})

character :: Lens (Entity a) a
character = lens _character (\c g -> g { _character = c})

data Pacman = Pacman deriving (Show, Eq)

data Ghost = Ghost deriving (Show, Eq)

data Gold = Gold deriving (Show, Eq)
