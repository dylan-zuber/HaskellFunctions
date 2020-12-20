-- Name: Dylan Zuber
-- Assignment: PA 6
-- Course/Semester: CS343/Fall2020
-- Lab Section: N/A
-- Sources Consulted: N/A

binaryToDecimal :: Int -> Int
binaryToDecimal 0 = 0
binaryToDecimal x = 2 * binaryToDecimal (x `div` 10) + (x `mod` 10)

addBinary' :: [Int] -> Int
addBinary' [] = 0
addBinary' (x:xs) = binaryToDecimal x + addBinary' xs

last' :: [a] -> a
last' [] = error "You passed an empty list :-("
last' [x] = x
last' (x:xs) = last' xs

init' :: [a] -> [a]
init' [] = error "You passed an empty list :-("
init' [x] = []
init' (x:xs) = x : init' xs

palindrome :: [Char] -> Bool
palindrome [] = True
palindrome [x] = True
palindrome (x:xs) | x == last' xs = palindrome (init' xs)
                 | otherwise = False

nTimes :: Int -> (Int -> Int) -> [Int]
nTimes n f = [f x | x <- [0..n-1]]

filterOut :: (a -> Bool) -> [a] -> [a]
filterOut x [] = []
filterOut x (n:xs) | x n = filterOut x xs
                   | otherwise = n : filterOut x xs

count :: (a -> Bool) -> [a] -> Int
count x [] = 0
count x (n:xs) | x n == True = 1 + count x xs
               | otherwise = count x xs

insert :: (Ord a) => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs) | n <= x = n : x : xs
                | otherwise = x : insert n xs

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x:xs) = insert x (insertionSort xs)

combine :: (a -> a -> a) -> [a] -> [a] -> [a]
combine f [] [b] = []
combine f [a] [] = []
combine f (x:xs) (y:ys) = f x y : combine f xs ys

minimum' :: (Ord a) => [a] -> a
minimum' [a] = a
minimum' (x:y:xs) | x < y = minimum ( x : xs ) 
                  | otherwise = minimum ( y : xs )

allTrue :: [Bool] -> Bool
allTrue [] = True
allTrue [a] = a
allTrue (x:xs) | x = allTrue xs
               | otherwise = False

anyTrue :: [Bool] -> Bool
anyTrue [] = False
anyTrue [a] = a
anyTrue (x:xs) | x = True
               | otherwise = anyTrue xs 

replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' n _ = []
replicate' n x | n > 0 = x : replicate' (n-1) x

lengths :: [[Char]] -> [Int]
lengths [] = []
lengths (x:xs) = length x : lengths xs 

divisors :: [Int] -> [[Int]]
divisors [] = []
divisors (x:xs) = [ n | n <- [1..x], x `mod` n == 0 ] : divisors xs

prime :: [Int] -> [Bool]
prime [] = []
prime (x:xs) = help x : prime xs

help n = if length ([ x | x <- [1..n], n `mod` x == 0 ]) == 2 then True else False


sumSq :: [Int] -> Int
sumSq [] = 0
sumSq (x:xs) = x * x + sumSq xs 