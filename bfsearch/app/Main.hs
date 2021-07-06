module Main where

import Lib
import Data.Matrix
import Data.Maybe

main :: IO ()
main = do
    m <- readPuzzles $ zero 9 9 
    print "question: "
    print m
    print "answer: "
    print $ fromJust $  bfsearch m (getBlanks m)
    -- print $ isValid m


readPuzzles :: Matrix Int -> IO (Matrix Int)
readPuzzles m = do
    contents <- getContents
    let thress = map (map read . words) $ lines contents
    return $ foldl (\m (x:y:v:_) -> setElem v (x,y) m) m thress
