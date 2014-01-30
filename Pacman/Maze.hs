module Pacman.Maze where

import Data.Lens.Common

newtype Maze = Maze { cells :: [[Cell]] } deriving Show

newtype Wall = Wall { isWalled :: Bool } deriving Show

data Cell = Cell
    { _north :: Wall
    , _south :: Wall
    , _east :: Wall
    , _west :: Wall
    , _hasGold :: Bool
    } deriving Show

north :: Lens Cell Wall
north = lens _north (\w c -> c { _north = w })

south :: Lens Cell Wall
south = lens _south (\w c -> c { _south = w })

east :: Lens Cell Wall
east = lens _east (\w c -> c { _east = w })

west :: Lens Cell Wall
west = lens _west (\w c -> c { _west = w })

unWalled :: Wall
unWalled = Wall False

walled :: Wall
walled = Wall True

emptyCell :: Cell
emptyCell = Cell unWalled unWalled unWalled unWalled False

emptyMaze :: Int -> Int -> Maze
emptyMaze c r = Maze . replicate r . replicate c $ emptyCell

addWall :: Lens Cell Wall -> Cell -> Cell
addWall l = l ^= walled
