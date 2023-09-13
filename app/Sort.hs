module Sort
( quickSort
, quickSort2
, mergeSort
) where

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let smallerSorted = quickSort [v | v <- xs, v <= x]
        biggerSorted = quickSort [v | v <- xs, v > x]
    in smallerSorted ++ [x] ++ biggerSorted

quickSort2 :: (Ord a) => [a] -> [a]
quickSort2 [] = []
quickSort2 (x:xs) = quickSort2 (filter (<=x) xs) ++ [x] ++ quickSort2 (filter (>x) xs)

-- Merge sort
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
    where
        (left, right) = splitAt (length xs `div` 2) xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] x = x
merge x [] = x
merge (x:xs) (y:ys)
    | x < y = [x] ++ merge xs (y:ys)
    | otherwise = [y] ++ merge (x:xs) ys

