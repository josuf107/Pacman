module Pacman where

import Control.Monad
import Graphics.UI.GLUT.Begin
import Graphics.UI.GLUT.Callbacks
import Graphics.UI.GLUT.Initialization
import Graphics.UI.GLUT.Window
import Graphics.Rendering.OpenGL
import System.Random

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
run = do
    void $ initialize "Pacman" []
    createWindow "Pacman"
    keyboardCallback $= Just handleInput
    displayCallback $= display
    mainLoop

display :: IO ()
display = do
    clear [ColorBuffer]
    renderPrimitive Triangles $ do
        mapM_ vertex . concat . fmap mkTriangle . take 100 $ zip (randoms . mkStdGen $ 4) (randoms . mkStdGen $ 5)
    swapBuffers

mkTriangle :: (GLfloat, GLfloat) -> [Vertex2 GLfloat]
mkTriangle (x, y) = [Vertex2 x y, Vertex2 (x - 0.01) y, Vertex2 x (y - 0.01)]

handleInput :: KeyboardCallback
handleInput 'q' _ = do
    mw <- get currentWindow
    case mw of
        Just w -> destroyWindow w
        Nothing -> putStrLn "No current window!"
handleInput c _ = putStrLn [c]
