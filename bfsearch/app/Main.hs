module Main where

import Lib
import Data.Matrix
import Data.Maybe
import Control.Monad.State

main :: IO ()
main = do
    m <- readPuzzles $ zero 9 9 
    print "question: "
    print m
    print "answer: "
    let blanks = getBlanks m
        nums = findValidElem m blanks
    print $ fromJust $  bfsearch m blanks nums
    print $ (runState (bfsearchState m blanks nums) 0)
    -- print $ isValid m


readPuzzles :: Matrix Int -> IO (Matrix Int)
readPuzzles m = do
    contents <- getContents
    let thress = map (map read . words) $ lines contents
    return $ foldl (\m (x:y:v:_) -> setElem v (x,y) m) m thress
