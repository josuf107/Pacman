module Pacman.Maze where

import Data.Function
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
fullMaze x y = walkMaze mazeFiller $ emptyMaze x y

addWall :: Point -> Point -> Maze -> Maze
addWall p1 p2 m = if S.member p1 ps && S.member p2 ps
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

data MazeWalker s = MazeWalker
    { point :: Point
    , nexts :: MazeWalker s -> Maze -> [Point]
    , step :: Point -> MazeWalker s -> Maze -> (MazeWalker s, Maze)
    , state :: s
    }

walkMaze :: MazeWalker s -> Maze -> Maze
walkMaze mw m =
    case filter (m `hasCell`) (nexts mw mw m) of
        (n:_) -> uncurry walkMaze (step mw n mw m)
        _ -> m

simpleWalker :: Point
    -> (MazeWalker () -> Maze -> Maze)
    -> (MazeWalker () -> Maze -> [Point])
    -> MazeWalker ()
simpleWalker p a ns = MazeWalker
    p
    ns
    (\p' mw m -> (mw { point = p'}, a mw m))
    ()

mazeFiller :: MazeWalker ()
mazeFiller = simpleWalker (0, 0) doFill getNexts
    where
        doFill :: MazeWalker () -> Maze -> Maze
        doFill mw =
            let p@(x, y) = point mw in
                addWall p (x + 1, y) . addWall p (x, y + 1)
        getNexts :: MazeWalker () -> Maze -> [Point]
        getNexts mw _ =
            let (x, y) = point mw in
                [(x + 1, y), (0, y + 1)]

mazeGouger :: RandomGen g => g -> Double -> MazeWalker (Direction, g)
mazeGouger gInitial r = MazeWalker
    (0, 0)
    getNexts
    (\p mw m -> (doStep p m mw, doGouge p mw m))
    (East, gInitial)
    where
        getNexts :: RandomGen g => MazeWalker (Direction, g)
            -> Maze
            -> [Point]
        getNexts mw m =
            let p = point mw
                (d, g) = state mw
                (ps, _) = nextState g d m p
            in ps
        doStep :: RandomGen g => Point
            -> Maze
            -> MazeWalker (Direction, g)
            -> MazeWalker (Direction, g)
        doStep p' m mw =
            let p = point mw
                (d, g) = state mw
                (_, g') = nextState g d m p
            in mw { point = p', state = (relativeDirection p p', g') }
        nextState :: RandomGen g => g
            -> Direction
            -> Maze
            -> Point
            -> ([Point], g)
        nextState g d m p = if wallRatio m <= r then ([], g)
            else
                let
                    ns = L.intersect
                        (availableDirections m p)
                        [d, d, d, North, South, East, West]
                in
                case pick g (L.permutations ns) of
                    Just (d', g') -> (fmap (`relativePoint` p) d', g')
                    Nothing -> error "Impossibru!"
        doGouge :: Point -> MazeWalker (Direction, g) -> Maze -> Maze
        doGouge p' mw = removeWall (point mw) p'

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
