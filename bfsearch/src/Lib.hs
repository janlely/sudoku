module Lib where

import Data.Matrix
import Data.List
import Data.Maybe
import Control.Monad.State
import qualified Debug.Trace as T

bfsearchState :: Matrix Int -> [(Int, Int)] -> [Int] -> State Int (Maybe (Matrix Int))
bfsearchState mtx blanks nums = do
    cur <- get
    put (cur + 1)
    case blanks of
      [] -> return $ Just mtx
      (x,y):xys -> case nums of
                     [] -> return Nothing 
                     n:ns -> let newMtx = setElem n (x,y) mtx
                              in if isValid newMtx
                                    then liftM2 maybeOr (bfsearchState newMtx xys (findValidElem newMtx xys)) (bfsearchState mtx blanks ns) 
                                    else bfsearchState mtx blanks ns

maybeOr :: Maybe a -> Maybe a -> Maybe a
maybeOr Nothing a = a
maybeOr (Just a) _ = Just a

bfsearch :: Matrix Int -> [(Int, Int)] -> [Int] -> Maybe (Matrix Int)
bfsearch mtx blanks nums =
    case blanks of
      [] -> Just mtx
      (x,y):xys -> case nums of
                     [] -> Nothing
                     n:ns -> let newMtx = setElem n (x,y) mtx
                              in if isValid newMtx
                                    then maybeOr (bfsearch newMtx xys (findValidElem newMtx xys)) (bfsearch mtx blanks ns)
                                    else bfsearch mtx blanks ns
          
getBlanks :: Matrix Int -> [(Int, Int)]
getBlanks m = filter isZero [(x,y) | x <- [1..9], y <- [1..9]]
    where isZero (x,y) = getElem x y m == 0

findValidElem :: Matrix Int -> [(Int, Int)] -> [Int]
findValidElem _ [] = []
findValidElem mtx ((x,y):_) =
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
