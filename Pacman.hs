module Pacman where

import Pacman.Data
import qualified Pacman.Maze as Maze

import Data.Function
import Data.Lens.Common
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

initGame :: RandomGen g => g -> Game
initGame g = Game (Entity 0 0 0 0 Pacman) [] [] (Maze.gougedMaze g 1.8 10 10) 0 0

run :: IO ()
run = do
    g <- newStdGen
    play
        {-(InWindow "Pacman" (200, 200) (500, 500))-}
        (FullScreen (0,0))
        black
        22
        (initGame g)
        render
        handleInput
        step

render :: Game -> Picture
render game = Pictures [Color yellow
    $ Translate (p ^. xCoord) (p ^. yCoord)
    $ circleSolid 18
    , renderMaze $ game ^. maze]
    where p = game ^. player

renderMaze :: Maze.Maze -> Picture
renderMaze m =
    Scale 40 40
    . Translate (fromIntegral (Maze.width m) / (-2))
        (fromIntegral (Maze.height m) / (-2))
    . Color white
    . Pictures
    . zipWith (Translate 0) [0..]
    . fmap ( Pictures
        . zipWith (flip Translate 0) [0..]
        . fmap (renderCell m)
        . L.sortBy (compare `on` fst))
    . unorderedGroupBy snd
    . S.toAscList
    . Maze.cells
    $ m
    where
        unorderedGroupBy :: (Ord b, Eq b) => (a -> b) -> [a] -> [[a]]
        unorderedGroupBy f = L.groupBy ((==) `on` f)
            . L.sortBy (compare `on` f)

renderCell :: Maze.Maze -> Maze.Point -> Picture
renderCell m p = Pictures $
    (case (neighbor Maze.South, neighbor Maze.East) of
        (True, True) -> [bottom, right]
        (True, False) -> [bottom]
        (False, True) -> [right]
        (False, False) -> [])
    ++ [left | fst p == 0]
    ++ [right | fst p == (Maze.width m - 1)]
    ++ [bottom | snd p == 0]
    ++ [top | snd p == (Maze.height m - 1)]
    ++ [Polygon [(0,0),(0,1),(1,1),(1,0)] | Maze.numWalls m p == 4]
    where
        neighbor :: Maze.Direction -> Bool
        neighbor d = Maze.hasWall p (Maze.relativePoint d p) m
        bottom :: Picture
        bottom = Line [(0,0), (1,0)]
        right :: Picture
        right = Line [(1,0), (1,1)]
        left :: Picture
        left = Line [(0,0), (0,1)]
        top :: Picture
        top = Line [(0,1), (1,1)]

step :: Float -> Game -> Game
step t = player ^%= move t

move :: Float -> Entity a -> Entity a
move t e = (xCoord ^+= (e ^. xDelta * t))
    . (yCoord ^+= (e ^. yDelta * t)) $ e

handleInput :: Event -> Game -> Game
handleInput (EventKey (Char c) Down _ _) = handleChar c
handleInput _ = id

handleChar :: Char -> Game -> Game
handleChar c = fromMaybe id . M.lookup c $ charMap
    where
        charMap = M.fromList
            [ ('h', player ^%= moveBack xDelta)
            , ('l', player ^%= moveFore xDelta)
            , ('j', player ^%= moveBack yDelta)
            , ('k', player ^%= moveFore yDelta)
            ]

speed :: Float
speed = 200

moveFore :: Lens (Entity a) Float -> Entity a -> Entity a
moveFore l = (l ^= speed) . stop

moveBack :: Lens (Entity a) Float -> Entity a -> Entity a
moveBack l = (l ^= (-speed)) . stop

stop :: Entity a -> Entity a
stop = (yDelta ^= 0) . (xDelta ^= 0)
