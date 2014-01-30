module Pacman.Maze where

import Data.Lens.Common
import qualified Data.Map as M

newtype Point = Point (Int, Int) deriving (Show, Eq, Ord)

px :: Lens Point Int
px = lens (\(Point (x,_)) -> x) (\x (Point (_, y)) -> Point (x, y))

py :: Lens Point Int
py = lens (\(Point (_,y)) -> y) (\y (Point (x, _)) -> Point (x, y))

newtype Maze = Maze { _cells :: M.Map Point Cell} deriving Show

cells :: Lens Maze (M.Map Point Cell)
cells = lens _cells (\mpc m -> Maze mpc)

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
emptyMaze c r = Maze . M.fromList . zip points . repeat $ emptyCell
    where
        points = fmap Point [(x, y) | x <- [0..c], y <- [0..r]]

addWall :: Lens Cell Wall -> Cell -> Cell
addWall l = l ^= walled
