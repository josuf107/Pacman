module Pacman where

import Pacman.Data

import Control.Monad
import Data.Lens.Common
import Graphics.Gloss
import System.Random

initGame :: Game
initGame = Game (Entity 0 0 Pacman) [] [] 0 0

run :: IO ()
run = play
        (InWindow "Pacman" (200, 200) (200, 200))
        black
        30
        initGame
        render
        (const id)
        (const id)

render :: Game -> Picture
render game = Color yellow (Translate (p ^. xCoord) (p ^. yCoord) (circleSolid 20))
    where p = game ^. player

{-renderGame :: Game -> IO ()-}
{-renderGame game = renderPacman pacman-}
    {-where-}
        {-pacman = game ^. player-}

{-gl :: Float -> GLfloat-}
{-gl = realToFrac-}

{-color3 :: Float -> Float -> Float -> IO ()-}
{-color3 r g b = color $ Color3 (gl r) (gl g) (gl b)-}

{-renderPacman :: Entity Pacman -> IO ()-}
{-renderPacman (Entity x y p) = preservingColor $ do-}
    {-color3 1 1 0-}
    {-circle x y 0.1-}

{-renderPoints :: [(Float, Float)] -> IO ()-}
{-renderPoints = mapM_ (\(x, y) -> vertex $ Vertex2 (gl x) (gl y))-}

{-circle :: Float -> Float -> Float -> IO ()-}
{-circle x y r = renderPrimitive TriangleFan $ renderPoints circular-}
    {-where-}
        {-circular = map (\t -> (x+r*cos (t), y+r*sin (t))) [0,0.2..(2*pi)]-}

{-preservingColor :: IO a -> IO a-}
{-preservingColor am = do-}
    {-c <- get currentColor-}
    {-a <- am-}
    {-currentColor $= c-}
    {-return a-}

{-mkTriangle :: (GLfloat, GLfloat) -> [Vertex2 GLfloat]-}
{-mkTriangle (x, y) = [Vertex2 x y, Vertex2 (x - 0.01) y, Vertex2 x (y - 0.01)]-}

{-handleInput :: IORef Game -> KeyboardCallback-}
{-handleInput gameRef 'h' _ = modifyIORef gameRef moveLeft >> display gameRef-}
{-handleInput gameRef 'l' _ = modifyIORef gameRef moveRight >> display gameRef-}
{-handleInput gameRef 'j' _ = modifyIORef gameRef moveDown >> display gameRef-}
{-handleInput gameRef 'k' _ = modifyIORef gameRef moveUp >> display gameRef-}
{-handleInput gameRef 'q' _ = do-}
    {-mw <- get currentWindow-}
    {-case mw of-}
        {-Just w -> destroyWindow w-}
        {-Nothing -> putStrLn "No current window!"-}
{-handleInput gameRef c _ = putStrLn [c]-}

{-speed :: Float-}
{-speed = 0.01-}

{-moveLeft :: Game -> Game-}
{-moveLeft = player ^%= (xCoord ^-= speed)-}

{-moveRight :: Game -> Game-}
{-moveRight = player ^%= (xCoord ^+= speed)-}

{-moveUp :: Game -> Game-}
{-moveUp = player ^%= (yCoord ^+= speed)-}

{-moveDown :: Game -> Game-}
{-moveDown = player ^%= (yCoord ^-= speed)-}
