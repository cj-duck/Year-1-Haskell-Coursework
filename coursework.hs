{-
Author: Christopher Johnson
Matric: 40275286
Date of Last Modification: 06/04/2017
-}

{-
READ ME: Change the name of this file to YOURSTUDENTNUMBER.hs. For example, if your
student number were 123456789, then you would rename the file 123456789.hs.

REPLACE the function definitions for each of the questions. 
The names of the functions correspond to the names given in the document set07016_cwk_16_17.pdf. 

DO NOT CHANGE THE TYPE SIGNATURES, OR THE ORDER OF THE ARGUMENTS!

You may add as many extra or helper functions as you wish, but do not make any "import" statements.
-}

-- QUESTION 1: Sets

complement :: (Eq a) => [a] -> [a] -> Maybe [a]
complement xs ys = error "You've not tried to write complement yet"

mUnion :: (Eq a) => [a] -> [a] -> [a]
mUnion xs ys = ( xs ++ ys )

-- TEST SET FOR Q1
{-
Your functions should have the following behaviour:
complement [1,2,3] [1..5] = Just [4,5]
complement [1,2,3] [2..5] = Nothing
mUnion [1,1,1,2] [1,2,2,3,4,4] = [1,1,1,2,2,3,4,4]
mUnion [1,4,1] [4,4,1] = [1,1,4,4]

THE ORDER OF ELEMENTS IN THE RESULTS OF mUnion IS NOT IMPORTANT.
-}



-- QUESTION 2: Functions and relations

reflClosure :: (Eq a) => [(a,a)] -> [(a,a)]
reflClosure (x:xs) = nub ( (x:xs) ++ [ (x,x) | x <- (heads (x:xs)) ++ (tails (x:xs))])

nub :: Eq a => [a] -> [a]
nub = nubBy (==)

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy eq [] = []
nubBy eq (x:xs) = x : nubBy eq (filter (\y -> not (eq x y)) xs)

heads :: (Eq a) => [(a,a)] -> [a]
heads = nub . map fst

tails :: (Eq a) => [(a,a)] -> [a]
tails = nub . map snd

exists :: (Eq a) => a -> [a] -> Bool
exists x xs = length (filter (==x) xs) > 0

-- TEST SET FOR Q2
{-
Your functions should have the following behaviour:
reflClosure [(1,2),(3,2)] = [(1,2),(3,2),(1,1),(2,2),(3,3)]
reflClosure [(1,1),(3,5)] = [(1,1),(3,5),(3,3),(5,5)]

DO NOT WORRY ABOUT THE ORDER IN WHICH PAIRS APPEAR IN YOUR LIST
-}



-- QUESTION 3: Combinatorics

choose2 :: [Int] -> [(Int,Int)]
choose2 xs = [(x,y) | x <- xs, y <- xs, x < y]

-- TEST SET FOR Q3
{-
Your functions should have the following behaviour:
choose2 [1,2,3] = [(1,2),(1,3),(2,3)]
choose2 [2,6,9,12] = [(2,6),(2,9),(2,12),(6,9),(6,12),(9,12)]
NOTE THAT THE SMALLER ELEMENT IN EACH PAIR APPEARS FIRST. THE ORDERING OF THE PAIRS IN THE LIST DOES NOT MATTER.
-}




-- QUESTION 4: Primes

factors :: Int -> [Int]
factors n = [x | x <- [2..n], n `mod` x == 0]

primeFactorisation :: Int -> [Int]
primeFactorisation 1 = []
primeFactorisation n
        | factors == []  = [n]
        | otherwise = factors ++ primeFactorisation (n `div` (head factors))
             where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

isPrime :: Int -> Bool
isPrime x   | x < 2 = False
        | otherwise = prime (2:[3,4..(x-1)])
     where 
     prime (y:z)
        | x < y ^ 2 = True
        | x `mod` y == 0 = False
        | otherwise = prime z

nextPrime :: Int -> Int
nextPrime n | isPrime n = n  
            | otherwise = nextPrime (n+1)

-- TEST SET FOR Q4
{-
Your functions should have the following behaviour:
factors 75 = [3,5,15,25,75]
factors 64 = [2,4,8,16,32,64]
primeFactorisation 75 = [3,5,5]
primeFactorisation 64 = [2,2,2,2,2,2]
nextPrime 25 = 29
nextPrime 400 = 401
-}




-- QUESTION 5: RSA

eTotient :: Int -> Int
eTotient n = error "You've not tried to write eTotient yet"

encode :: Int -> Int -> Int -> Int -> Maybe Int
encode p q m e = error "You've not tried to write encode yet"

-- TEST SET FOR Q5
{-
Your functions should have the following behaviour:
eTotient 54 = 18
eTotient 73 = 72
encode 53 73 151 95 = Just 3689
encode 99 18 108 45 = Nothing
encode 37 17 23 48 = Nothing
-}


