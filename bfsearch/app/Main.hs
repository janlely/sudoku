module Main where

import Lib
import Data.Matrix

main :: IO ()
main = do
    m <- readPuzzles $ zero 9 9 
    print m
    -- print $ bfsearch m (getBlanks m)
    -- print $ isValid m

