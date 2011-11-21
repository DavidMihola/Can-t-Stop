-- enthalten in missingh
import Data.List.Utils (hasAny)

import Data.List ((\\), sortBy)

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:y  | x <- xs, y <- cp xss]

dice_values :: [Int]
dice_values = [1..6]

all_combinations :: [[Int]]
all_combinations = cp $ take 4 $ repeat $ dice_values

partial_sums :: [Int] -> [Int]
partial_sums (a:b:c:d:[]) = [a + b, a + c, a + d, b + c, b + d, c + d]

all_partial_sums :: [[Int]]
all_partial_sums = map partial_sums all_combinations

nr_of_combinations :: Int
nr_of_combinations = length all_combinations

absolute :: ([Int] -> Bool) -> Int
absolute f = length $ filter f all_partial_sums

percentage f = abc / xyz * 100
  where abc = fromIntegral $ absolute f
     	xyz = fromIntegral nr_of_combinations

take_x_of :: Int -> [a] -> [[a]]
take_x_of 1 xs = [ [y] | y <- xs]
take_x_of i xs = if (i == (length xs)) then [xs] else
           [ y:ys | (y,rest) <- first_and_rest xs, ys <- take_x_of (i-1) rest]

first_and_rest :: [a] -> [(a,[a])]
first_and_rest [] = []
first_and_rest (x:xs) = (x, xs) : first_and_rest xs

three_numbers = take_x_of 3 [2..12]

numbers_and_probs = sortBy comparePairs [ (i, p) | i <- three_numbers, let p = (percentage . hasAny) i]

comparePairs :: Ord a => (b,a) -> (b,a) -> Ordering
comparePairs (_,x) (_,y) = compare x y
