module Pacman.Test where

import Pacman.Maze

import Test.HUnit
import Test.QuickCheck

runTests :: IO Counts
runTests = do
    mapM quickCheck enabledChecks
    runTestTT enabledTests

enabledTests :: Test
enabledTests = TestList
    [ testAddWall
    ]

enabledChecks :: [Property]
enabledChecks =
    [ checkMazeSize
    ]

labeled :: String -> Assertion -> Test
labeled l a = TestLabel l $ TestCase a

testAddWall :: Test
testAddWall = TestCase $
    assertBool "add wall"
        (hasWall (0,0) (0,1)
        . addWall (0,0) (0,1)
        $ emptyMaze 1 1)

checkMazeSize :: Property
checkMazeSize = label "maze size" (
    \(x, y) -> let e = emptyMaze x y in
        width e == x + 1 && height e == y + 1)
