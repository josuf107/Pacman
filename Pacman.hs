module Pacman where

import Pacman.Data

import Data.Lens.Common
import qualified Data.Map as M
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

initGame :: Game
initGame = Game (Entity 0 0 0 0 Pacman) [] [] 0 0

run :: IO ()
run = play
        {-(InWindow "Pacman" (200, 200) (500, 500))-}
        (FullScreen (0,0))
        black
        22
        initGame
        render
        handleInput
        step

render :: Game -> Picture
render game = Color yellow
    $ Translate (p ^. xCoord) (p ^. yCoord)
    $ circleSolid 20
    where p = game ^. player

step :: Float -> Game -> Game
step t = player ^%= move t

move :: Float -> Entity a -> Entity a
move t e = (xCoord ^+= (e ^. xDelta * t))
    . (yCoord ^+= (e ^. yDelta * t)) $ e

handleInput :: Event -> Game -> Game
handleInput (EventKey (Char c) Down _ _) = lookupChar c
handleInput _ = id

lookupChar :: Char -> Game -> Game
lookupChar c = fromMaybe id . M.lookup c $ charMap
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
