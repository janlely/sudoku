module Lib where

import Data.Matrix
import Data.List

solveSudoku :: Matrix Int ->  Matrix Int
-- solveSudoku = trialAndError . applyRule2 . applyRule1
solveSudoku = applyRule1


readPuzzles :: Matrix Int -> IO (Matrix Int)
readPuzzles m = do
    line <- getLine
    if "" == line
       then return m
       else let x1:x2:x3:_ = parseInput line
                m'         = setElem x3 (x1, x2) m
             in readPuzzles m' 

parseInput :: String -> [Int] 
parseInput = fmap read  . words 

applyRule1 :: Matrix Int -> Matrix Int
applyRule1 m =
    let (m', changed) = foldl foldFunc4 (m, False) [(x,y)| x <- [0..2], y <- [0..2]]
     in if not changed
           then m
           else applyRule1 m'

foldFunc4 :: (Matrix Int, Bool) -> (Int, Int) -> (Matrix Int, Bool)
foldFunc4 (mtx, changed) (m, n) =
    let (mtx', b) = crossCheckMN mtx m n
     in case b of
          False -> (mtx', changed)
          True -> (mtx', True)

gridMN :: Int -> Int -> [(Int, Int)]
gridMN m n = [(m * 3 + x, n * 3 + y) | x <- [1..3], y <- [1..3]]

neighborMN :: Int -> Int -> [(Int, Int)]
neighborMN m n =
    let horizontal = [(x + m*3, y)|x <- [1..3], y <- [1..9], y /= 1 + n*3,  y /= 2 + n*3, y /= 3 + n*3]
        vertical   = [(x, y + n*3)|x <- [1..9], y <- [1..3], x /= 1 + m*3,  x /= 2 + m*3, x /= 3 + m*3]
     in horizontal ++ vertical



crossCheckMN :: Matrix Int -> Int -> Int -> (Matrix Int, Bool)
crossCheckMN mtx m n        =
    let (blanks, nonBlanks) = foldl (foldFunc mtx) ([], []) (gridMN m n)
        neighbors           = neighborMN m n
     in foldl (foldFunc2 blanks neighbors m n) (mtx, False) (filter (`notElem` nonBlanks) [1..9])


foldFunc2 :: [(Int, Int)] -> [(Int, Int)] -> Int -> Int -> (Matrix Int, Bool) -> Int -> (Matrix Int, Bool)
foldFunc2 blks nbs m n (mtx, changed) x =
    let (rows, cols) = foldl (foldFunc3 mtx x) ([], []) nbs
        blks' = filter (\(bh,bv) -> notElem bh rows && notElem bv cols) blks
     in case blks' of
          (r,v):[] -> (setElem x (r,v) mtx, True)
          _        -> (mtx, changed)

        
foldFunc3 :: Matrix Int -> Int -> ([Int], [Int]) -> (Int, Int) -> ([Int],[Int])
foldFunc3 mtx n (rs, cs) (x,y) =
    let v = getElem x y mtx
     in if v == n
           then (x:rs, y:cs)
           else (rs, cs)


foldFunc :: Matrix Int -> ([(Int, Int)], [Int]) -> (Int, Int) -> ([(Int, Int)], [Int])
foldFunc m (xs, ys) (x,y) =
    case getElem x y m of
      0 -> ((x,y):xs, ys)
      v -> (xs, v:ys)

