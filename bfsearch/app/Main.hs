module Main where

import Lib
-- import Graphics.Gloss
--
-- window :: Display
-- window = InWindow "Nice Window" (200, 200) (10, 10)
--
-- background :: Color
-- background = white
--
-- gridPoints :: Path
-- gridPoints = [(10 + x * 20, 10 + y * 20) | x <- [0..9], [0..9]]
--
-- drawing :: Picture
-- drawing = polygon gridPoints
--
-- main :: IO ()
-- main = display window background drawing
--
main :: IO ()
main = solveSudoku


