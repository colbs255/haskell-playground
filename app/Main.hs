module Main where

listLength :: [a] -> Integer
listLength []     = 0
listLength (_:xs) = 1 + listLength xs

twoSum :: [Integer] -> Integer -> [(Integer, Integer)]
twoSum a target = [(i,j) | i <- a, j <- a, i + j == target]

max' :: (Ord a) => a -> a -> a
max' a b
    | a < b = b
    | otherwise  = a

maxInList :: (Ord a) => [a] -> a
maxInList [] = error "list is empty"
maxInList [x] = x
maxInList (x:xs)
    | x < rest = rest
    | otherwise = x
    where rest = maxInList xs

myRepeat :: n -> Integer -> [n]
myRepeat v count
    | count <= 0 = []
    | otherwise = v:(myRepeat v (count-1))

myTake :: [a] -> Integer -> [a]
myTake _ count
    | count <= 0 = []
-- accounts for case in which count is greater than size of list
myTake [] _ = []
myTake (x:xs) count = x:(myTake xs (count-1))


myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y):myZip xs ys

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
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] x = x
merge x [] = x
merge (x:xs) (y:ys)
    | x < y = [x] ++ merge xs (y:ys)
    | otherwise = [y] ++ merge (x:xs) ys

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
    where
        (left, right) = splitAt (length xs `div` 2) xs

-- Lambda
mySum :: (Num a) => [a] -> a
-- mySum xs = foldl (\acc v -> acc + v) 0 xs
mySum = foldl (+) 0

sumSquares :: (Num a) => [a] -> a
-- sumSquares xs = sum (map (^2) xs)
sumSquares = sum . map (^2)

main :: IO ()
main = do
    let example = [1..4]
    print (listLength example)
    print (twoSum example 5)
    print (max 5 6)
    print (maxInList [1..10])
    print (myRepeat 'a' 5)
    print (myTake [1..5] 3)
    print (myZip [1..5] ['a', 'b', 'c', 'd', 'e'])
    print (quickSort [4, 3, 2, 1])
    print (quickSort2 [4, 3, 2, 1])
    print (merge [1, 3, 6] [2, 4, 5])
    print (mergeSort [1, 3, 6, 2, 4, 5])
    print (mySum [1, 2, 3])
    print (sumSquares [1, 2, 3])

    -- suim

