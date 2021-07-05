module Main where

import Lib
import Data.Matrix;

main :: IO ()
main = do
    m <- readPuzzles $ zero 9 9
    print m
    print $ solveSudoku m
    -- print $ neighborMN 2 0
    -- print $ foldl (foldFunc m) ([], []) (gridMN 2 0)

