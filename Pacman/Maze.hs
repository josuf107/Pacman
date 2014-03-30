module Pacman.Maze where

import Control.Applicative
import Data.Function
import Data.Monoid
import qualified Data.List as L
import qualified Data.Set as S
import System.Random

data Direction = North | East | South | West deriving (Show, Eq)
type Point = (Int, Int)

data Maze = Maze
    { walls :: S.Set (Point, Point)
    , cells :: S.Set Point
    , height :: Int
    , width :: Int
    } deriving (Show, Eq)

printMaze :: Maze -> String
printMaze m = id
    . L.intercalate "\n"
    . fmap (concat
        . fmap (drawCell m)
        . L.sortBy (compare `on` fst))
    . unorderedGroupBy snd
    . S.toAscList
    . cells
    $ m
    where
        unorderedGroupBy :: (Ord b, Eq b) => (a -> b) -> [a] -> [[a]]
        unorderedGroupBy f = L.groupBy ((==) `on` f)
            . L.sortBy (compare `on` f)

drawCell :: Maze -> Point -> String
drawCell m p =
    case (neighbor South, neighbor East) of
        (True, True) -> "__|"
        (True, False) -> "__ "
        (False, True) -> "  |"
        (False, False) -> "   "
    where
        neighbor :: Direction -> Bool
        neighbor d = hasWall p (relativePoint d p) m

emptyMaze :: Int -> Int -> Maze
emptyMaze mx my = Maze
    S.empty
    (S.fromList [(x, y) | x <- [0..mx], y <- [0..my]])
    (my + 1)
    (mx + 1)

fullMaze :: Int -> Int -> Maze
fullMaze x y = fill $ emptyMaze x y
    where
        fill :: Maze -> Maze
        fill m =
            (appEndo . mconcat . fmap Endo . concat
            . fmap (\c -> addWall c <$> neighbors c)
            . S.toList . cells
            $ m) m

addWall :: Point -> Point -> Maze -> Maze
addWall p1 p2 m =
    if S.member p1 ps && S.member p2 ps && p1 `elem` neighbors p2
    then m { walls = S.insert (p2, p1) . S.insert (p1, p2) . walls $ m }
    else m
    where
        ps :: S.Set Point
        ps = cells m

removeWall :: Point -> Point -> Maze -> Maze
removeWall p1 p2 m = m { walls = S.delete (p2, p1)
    . S.delete (p1, p2)
    . walls
    $ m }

hasWall :: Point -> Point -> Maze -> Bool
hasWall p1 p2 m = S.member (p1, p2) ws
    || S.member (p2, p1) ws
    || (outOfBounds p1 && S.member p2 cs && p2 `elem` neighbors p1)
    || (outOfBounds p2 && S.member p1 cs && p1 `elem` neighbors p2)
    where
        ws = walls m
        cs = cells m
        outOfBounds (-1,k) = k >= 0
        outOfBounds (k,-1) = k >= 0
        outOfBounds _ = False

hasCell :: Maze -> Point -> Bool
hasCell m =  (`S.member` cells m)

gougedMaze :: RandomGen g => g -> Double -> Int -> Int -> Maze
gougedMaze g r x y = gougeMaze g r South (0, 0) $ (fullMaze x y)
    where
        gougeMaze :: RandomGen g => g -> Double -> Direction -> Point -> Maze -> Maze
        gougeMaze g r d p m =
            if wallRatio m <= r then m
            else
                let
                    ns = L.intersect
                        (availableDirections m p)
                        [d, d, d, North, South, East, West]
                in
                case pick g (L.permutations ns) of
                    Just ([], g') -> m
                    Just ((d':_), g') -> gougeMaze
                        g' r d'
                        (relativePoint d' p)
                        (removeWall p (relativePoint d' p) m)
                    Nothing -> m

wallRatio :: Maze -> Double
wallRatio m = wallCount / (fromIntegral . S.size . cells $ m)
    where
        wallCount = fromIntegral $ (S.size . walls $ m)
            + 2 * height m
            + 2 * width m

neighbors :: Point -> [Point]
neighbors p = fmap (`relativePoint` p) [North, South, East, West]

availableDirections :: Maze -> Point -> [Direction]
availableDirections m p =
    if numWalls m p < 2 then
        freeSpaces m p
    else
        let
            (walled, unwalled) =
                L.partition (\p' -> hasWall p p' m)
                . filter (`S.member` cells m)
                . neighbors
                $ p
        in
            fmap
                (relativeDirection p)
                (unwalled ++ filter (\p' -> numWalls m p' > 2) walled)

numWalls :: Maze -> Point -> Int
numWalls m p = length
    . filter (\(p1, p2) -> hasWall p1 p2 m)
    . zip (repeat p)
    . neighbors
    $ p

freeSpaces :: Maze -> Point -> [Direction]
freeSpaces m p = fmap (uncurry relativeDirection)
    . filter (not . (\(p1, p2) -> hasWall p1 p2 m))
    . zip (repeat p)
    . filter (`S.member` cells m)
    . neighbors
    $ p

relativePoint :: Direction -> Point -> Point
relativePoint North (x, y) = (x, y - 1)
relativePoint South (x, y) = (x, y + 1)
relativePoint East (x, y) = (x + 1, y)
relativePoint West (x, y) = (x - 1, y)

relativeDirection :: Point -> Point -> Direction
relativeDirection (x, y) (x', y')
    | y' < y = North
    | y' > y = South
    | x' > x = East
    | x' < x = West
    | otherwise = North -- they're on top of each other

pick :: RandomGen g => g -> [a] -> Maybe (a, g)
pick _ [] = Nothing
pick g l = Just (l !! i, g')
    where
        (i, g') = randomR (0, length l - 1) g
