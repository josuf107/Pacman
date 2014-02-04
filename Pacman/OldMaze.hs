module Pacman.OldMaze where

import Control.Monad
import Data.Function (on)
import Data.Lens.Common
import qualified Data.List as L
import qualified Data.Map as M
import Debug.Trace
import System.Random

newtype Point = Point (Int, Int) deriving (Show, Eq, Ord)

px :: Lens Point Int
px = lens (\(Point (x,_)) -> x) (\x (Point (_, y)) -> Point (x, y))

py :: Lens Point Int
py = lens (\(Point (_,y)) -> y) (\y (Point (x, _)) -> Point (x, y))

data Maze = Maze { _cells :: M.Map Point Cell
                 , _rows :: Int
                 , _columns :: Int
                 } deriving Show

printMaze :: Maze -> String
printMaze m = L.intercalate "\n" -- newlines between rows
    . fmap (concat -- join cells in each row
        . fmap (printCell . snd) -- print cells
        . L.sortBy (compare `on` ((^. px) . fst))) -- sort each row by column
    . L.groupBy (\(p, _) (p', _) -> p ^. py == p' ^. py) -- group by row
    . L.sortBy (compare `on` ((^. py) . fst)) -- sort by row
    $ M.toAscList (m ^. cells) -- get [(Point, Cell)] list

printCell :: Cell -> String
printCell c = first ++ second ++ third
    where
        cellWalls = fmap (^. direction) . filter isWalled . walls $ c
        first = if West `elem` cellWalls then "|" else " "
        second
            | North `elem` cellWalls && South `elem` cellWalls = "EE"
            | North `elem` cellWalls = "``"
            | South `elem` cellWalls = "__"
            | otherwise = "  "
        third = if East `elem` cellWalls then "|" else " "

cells :: Lens Maze (M.Map Point Cell)
cells = lens _cells (\cs m -> m { _cells = cs })

rows :: Lens Maze Int
rows = lens _rows (\r m -> m { _rows = r })

columns :: Lens Maze Int
columns = lens _columns (\c m -> m { _columns = c })

data Direction = North | South | East | West deriving (Show, Eq)

data Wall = Wall
    { isWalled :: Bool
    , _direction :: Direction
    } deriving Show

direction :: Lens Wall Direction
direction = lens _direction (\d w -> w { _direction = d })

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

unWalled :: Direction -> Wall
unWalled = Wall False

walled :: Direction -> Wall
walled = Wall True

emptyCell :: Cell
emptyCell = Cell
    (unWalled North)
    (unWalled South)
    (unWalled East)
    (unWalled West)
    False

emptyMaze :: Int -> Int -> Maze
emptyMaze = Maze M.empty

fullCell :: Cell
fullCell = Cell
    (walled North)
    (walled South)
    (walled East)
    (walled West)
    False

fullMaze :: Int -> Int -> Maze
fullMaze c r = (cells ^= cellMap) (emptyMaze c r)
    where
        points = fmap Point [(x, y) | x <- [0..c], y <- [0..r]]
        cellMap = M.fromList . zip points . repeat $ fullCell

data MazeWalker g = MazeWalker
    { maze :: Maze
    , position :: Point
    , dir :: Direction
    , gen :: g
    , step :: MazeWalker g -> MazeWalker g
    }

randomMaze :: RandomGen g => g -> Int -> Int -> Maze
randomMaze g c r = randomWalk g (Point (0, 0)) (fullMaze c r)

randomWalk :: RandomGen g => g -> Point -> Maze -> Maze
randomWalk g p m =
    if checkMaze m then m
    else
        let mnext = do
            c <- M.lookup p (m ^. cells)
            pick g (if length unWalled == 3
                then unWalled
                else unWalled ++ walled)
        in
            case mnext of
                Just (next, g') -> randomWalk g'
                    (move next p)
                    (knockdown (move next p) (opposite next) . knockdown p next $ m)
                Nothing -> error "yer dumb"
    where
        (walled, unWalled) = available p m
        opposite :: Lens Cell Wall -> Lens Cell Wall
        opposite l =
            case emptyCell ^. l ^. direction of
                North -> south
                South -> north
                East -> west
                West -> east

knockdown :: Point -> Lens Cell Wall -> Maze -> Maze
knockdown p l = cells ^%= M.update (Just . removeWall l) p

move :: Lens Cell Wall -> Point -> Point
move l =
    case emptyCell ^. l ^. direction of
        North -> py ^-= 1
        South -> py ^+= 1
        East -> px ^+= 1
        West -> px ^-= 1

pick :: RandomGen g => g -> [a] -> Maybe (a, g)
pick g [] = Nothing
pick g l = Just (l !! i, g')
    where
        (i, g') = randomR (0, length l - 1) g

available :: Point -> Maze -> ([Lens Cell Wall], [Lens Cell Wall])
available p m = (walled, unWalled)
    where
        (walled, unWalled) = case M.lookup p (m ^. cells) of
                Just c -> (fstLens ^%= removeUnavailable c)
                    . L.partition (isWalled . (c ^.))
                    $ wallLs
                Nothing -> ([], [])
        removeUnavailable :: Cell -> [Lens Cell Wall] -> [Lens Cell Wall]
        removeUnavailable c = filter (not . rules . (c ^.))
        rules :: Wall -> Bool
        rules w = or
            [ w ^. direction == North && p ^. py == 0
            , w ^. direction == South && p ^. py == m ^. rows
            , w ^. direction == East && p ^. px == m ^. columns
            , w ^. direction == West && p ^. px == 0
            ]

checkMaze :: Maze -> Bool
checkMaze m = (==0) . M.size . snd . M.partition checkCell $ m ^. cells

checkCell :: Cell -> Bool
checkCell c = wallCount >= 1 && wallCount < 4
    where
        wallCount = length
            . filter isWalled
            . walls
            $ c

walls :: Cell -> [Wall]
walls c = fmap (c ^.) wallLs

wallLs :: [Lens Cell Wall]
wallLs = [north, south, east, west]

addWall :: Lens Cell Wall -> Cell -> Cell
addWall l c = (l ^= walled (c ^. l ^. direction)) c

removeWall :: Lens Cell Wall -> Cell -> Cell
removeWall l c = (l ^= unWalled (c ^. l ^. direction)) c
