module Lib where

import Data.Matrix
import Data.List
import Data.Maybe
import Debug.Trace


readPuzzles :: Matrix Int -> IO (Matrix Int)
readPuzzles m = do
    contents <- getContents
    let thress = map (map read . words) $ lines contents
    return $ foldl (\m (x:y:v:_) -> setElem v (x,y) m) m thress

bfsearch :: Matrix Int -> [(Int, Int)] -> Maybe (Matrix Int)
bfsearch mtx blanks =
    case blanks of
      [] -> Just mtx
      (x,y):xys -> fromJust $ find isJust $ map func (findValidElem mtx x y)
          where func num = let newMtx = setElem num (x,y) mtx
                            in if not $ isValid newMtx
                                  then Nothing
                                  else bfsearch newMtx xys
          
getBlanks :: Matrix Int -> [(Int, Int)]
getBlanks m = filter isZero [(x,y) | x <- [1..9], y <- [1..9]]
    where isZero (x,y) = getElem x y m == 0

findValidElem :: Matrix Int -> Int -> Int -> [Int]
findValidElem mtx x y =
    let grids = [(m + (3 * ((x-1) `div` 3)), n + (3 * ((y-1) `div` 3))) | m <- [1..3], n <- [1..3]] 
        xs = [(x, n) | n <- [1..9]]
        ys = [(m, y) | m <- [1..9]]
        nums = foldl (\vs (xx,yy) -> getElem xx yy mtx:vs) [] $ grids ++ xs ++ ys
     in filter (not . (`elem` nums)) [1..9]


isValid :: Matrix Int -> Bool
isValid m =
    let rows = map (\n -> cycle [n]  `zip` [1..9]) [1..9]
        cols = map (\n -> [1..9] `zip` cycle [n]) [1..9]
        grids =
            let pluses = [(x,y) | x <- [0,3,6], y <- [0,3,6]]
                inits = [(x,y) | x <- [1..3], y <- [1..3]]
             in map (\(x1,y1) -> map (\(x2,y2) -> (x1+x2, y1+y2)) inits) pluses
        xValid = and $ noDuplicate rows 
        yValid = and $ noDuplicate cols
        gridValid = and $ noDuplicate grids
     in and [xValid, yValid, gridValid]
    where noDuplicate xs = map func xs
          func = allDifferent . (map (\(x,y) -> getElem x y m))

allDifferent :: [Int] -> Bool
allDifferent []     = True
allDifferent (x:xs) = if x /= 0 
                         then x `notElem` xs && allDifferent xs
                         else allDifferent xs
