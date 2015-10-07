import Data.List
import Data.Ord (comparing)
import Data.Function (on)
import System.Random
-- 01-10 {{{ =================================================================
-- 01. Find the last element of a list. ----------------------------------
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [x] = x
myLast (x:xs) = myLast xs
-- last = Prelude.last

-- 02. Find the last-but-one element. ------------------------------------
myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast [x] = error "List is too short"
myButLast (x:_:[]) = x
myButLast (x:xs) = myButLast xs
-- butLast = last . init

-- 03. Find the K'th element, beginning with k=1 -------------------------
elementAt :: Int -> [a] -> a
elementAt n = head . drop (n - 1)
-- errors at out-of-bounds and empty lists

-- 04. Find the number of elements in a list -----------------------------
myLength :: [a] -> Int
myLength = sum . map (const 1)
-- length = foldr (\_ n -> n + 1) 0

-- 05. Reverse a list ----------------------------------------------------
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
-- reverse = foldl (flip (:)) []
-- myReverse'' xs = foldr (\x fId empty -> fId (x : empty)) id xs []

-- 06. Find if a list is a palindrome ------------------------------------
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- 07. Flatten a nested list structure (recursively) ---------------------
data NestedList a = Elem a
                  | List [NestedList a]
                  deriving ( Show )
testNest :: NestedList Int
testNest = List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]
-- ^ We have to define a new datatype b/c homogenous lists
flatten :: NestedList a -> [a]
flatten (Elem x)      = [x]
flatten (List [])     = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
                                              -- ^ This constructor!
-- Cheated -- but close! Also `concatMap` works just fine.

-- 08. Eliminate consecutive duplicates of list elements -----------------
testCompress = "aabccccaaaddeeeee"
compress :: Eq a => [a] -> [a]
compress [x]  = [x]
compress (x:xs)
  | x == head xs = compress xs
  | otherwise    = x : compress xs
-- compress'  = map head . Data.List.group
-- compress'' [] = []
-- compress'' (x:xs) = x : (compress $ dropWhile (== x) xs)

-- 09. Pack consecutive duplicates into sublists. (like `group` !) -------
myGroup :: Eq a => [a] -> [[a]]
myGroup [] = []
myGroup (x:xs) = let (first, rest) = span (==x) xs
                  in (x:first) : myGroup rest
                  -- ^ cheated. BUT I DIDN'T KNOW SPAN
                  -- which splits a list on a predicate
-- pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

-- 10. Run-length encoding of a list. ------------------------------------
-- e.g. encode "aaaabccaadeeee" = [(4,'a'),(1,'b'),(2,'c'),(2,'a'),..]
testEncode :: [Int]
testEncode = do
  i <- [1..]
  replicate i i
encode :: Eq a => [a] -> [(Int, a)]
encode xs = zip (map length $ myGroup xs) (compress xs)
-- pointfree : map (\x -> (length x, head x)) . group
encode2 :: Eq a => [a] -> [(Int, a)]
encode2 = map (\x -> (length x, head x)) . group
-- }}}
-- 11-20 {{{ =================================================================
-- 11. Modified run-length encoding. -------------------------------------
-- Modify 10 so that if an element is alone, it's copied into result.
data Some a = Single a
            | Multiple Int a
            deriving (Read, Show)
testSome = "aaaabccaadeeee"

encodeModified :: Eq a => [a] -> [Some a]
encodeModified = map go . group
  where go [x] = Single x
        go x   = Multiple (length x) (head x)

-- 12. Decode a run-length encoded list from 11. -------------------------
decodeModified :: [Some a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = decode x ++ decodeModified xs
  where decode (Single z) = [z]
        decode (Multiple n z) = replicate n z

-- 13. Direct solution for run-length encoding (?) -----------------------
-- This one is confusing. SkIPPING FOR NOW

-- 14. Duplicate elements of a list --------------------------------------
dupli :: [a] -> [a]
dupli = concatMap (replicate 2)
-- dupli (x:xs) = x:x:dupli xs

-- 15. Replicate elements of a list x times ------------------------------
repli :: Int -> [a] -> [a]
repli n = concatMap (replicate n)

-- 16. Drop every n'th element from list ---------------------------------
dropEvery :: Int -> [a] -> [a]
dropEvery 0 xs = xs
dropEvery n xs = concat $
  zipWith (\i j ->
    if j `rem` n == 0
    then []
    else [i]
  ) xs [1..]

-- 17. Split a list into two parts on a given length. Don't use predefs --
splitOn :: Int -> [a] -> ([a], [a])
splitOn n xs = (take n xs, drop n xs) -- predefined predicates...
-- splitOn n [] = ([], []) -- CHEATING VERSION APPARENTLY W/O PREDS
-- splitOn n l@(x:xs)
--  | n > 0     = (x : ys, split xs (n - 1))
--  | otherwise = ([], l) -- cheated

-- 18. Extract a slice from a list, inclusive ----------------------------
slice :: Int -> Int -> [a] -> [a]
slice i k = drop (i - 1) . take k

-- 19. Rotate a list n places to the left (<) (Hint: use (++) & predefs --
alphabet = ['a'..'z']
rotate :: Int -> [a] -> [a]
rotate n xs
  | n < 0     = let n' = length xs + n
                 in drop n' xs ++ take n' xs
  | n == 0    = xs
  | otherwise = drop n xs ++ take n xs

-- 20. Remove k'th element from a list -----------------------------------
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (head $ drop (n - 1) xs, take (n-1) xs ++ drop n xs)
--OR
removeAt' 1 (x:xs) = (x, xs)
removeAt' n (x:xs) = (l , x:r)
  where (l, r) = removeAt (n - 1) xs
-- }}}
-- {{{ 21-30 (28, really, for some reason) ===================================
-- 21. Insert an element at a given position into a list ---------------------
-- e.g. insertAt 'X' 2 "abcd" = "aXbcd"
insertAt :: a -> Int -> [a] -> [a]
insertAt x 1 xs = x:xs
insertAt x n (x':xs) = x':insertAt x (n - 1) xs

-- 22. Create a list containing all integers in a given range ----------------
range :: Int -> Int -> [Int]
range n m
  | n > m     = []
  | otherwise = n : range (n + 1) m

-- 23. Extract a given number of randomly selected elements from a list ------
randomSelect :: Int -> [a] -> IO [a]
-- NOTE: TOTALLY CHEATED
randomSelect n xs = do
  gen <- getStdGen
  return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]

-- 24-25 : Random problems. As in, problems involving random. ----------------
-- ....... so I'm not doing them. meh. TODO ----------------------------------

-- 26. Generate the combinations of K objects from a list --------------------
-- eg. in how many ways can a committee of 3 be chosen from a group of 12? we
--     all know that there are C(12,3) = 220 possibilities (binomial coeffici-
--     ents). But what are all 220 different combinations?
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys    <- combinations (n - 1) xs' ]

-- 27. Group the elements of a set into disjoint subsets ---------------------
-- I'm not even sure what this means. So leaving for now. TODO  --------------

-- 28. Sort a list of lists according to the length of sublists
--  a. sort short to long.
listList :: [[Char]]
listList = ["abc","de","fgh","de","ijkl","mn","o"]
lsort :: [[a]] -> [[a]]
lsort = sortBy (\lx ly -> length lx `compare` length ly)
-- OR FUCKING THIS: (with 'comparing' from Data.Ord)
lsort' = sortBy (comparing length) -- which is the same.
-- OR:
--lsort'' = sortOn length -- sortOn == sortBy . comparing (but evaluates
                        --                             elements only once)

--  b. Sort by 'length frequency' : lists with rare lengths are first, then
--     more frequently-occuring lists.
lfsort :: [[a]] -> [[a]]
lfsort = concat . lsort . groupBy (\lx ly -> length lx == length ly) . lsort
-- OR (with 'on' from Data.Function)
lfsort' = concat . lsort . groupBy ((==) `on` length) . lsort
-- OR what I was originally thinking:
-- lfsort'' = map snd
--          . concat
--          . sortOn length
--          . groupBy ((==) `on` fst)
--          . sortOn fst
--          . map (\x -> (length x, x))

-- }}}
-- {{{ 31-41   ===============================================================
-- 31. Determine whether a given integer is prime  ---------------------------
isPrime :: Int -> Bool
isPrime 1 = False
isPrime x = let ys = [2..x']
                x' = floor . sqrt . fromIntegral $ x
                divides i j = i `rem` j == 0
             in not $ any (divides x) ys


primeTest :: [Int]
primeTest = filter isPrime [1..100]

-- 32. Determine the greatest common divisor of 2 integers using Euclid ------
myGCD :: Int -> Int -> Int
myGCD x y
  | x `rem` y == 0 = abs y
  | y `rem` x == 0 = abs x
  | x > y          = myGCD y (x `rem` y)
  | x < y          = myGCD x (y `rem` x)

-- 33. Find whether 2 ints are coprime  --------------------------------------
coprime :: Int -> Int -> Bool
coprime x y = gcd x y == 1

-- 34. Find the totient (number of coprimes) of x ----------------------------
totient :: Int -> Int
totient 1 = 1
totient x = length $ filter (coprime x) [1..(x - 1)]
-- OR
{- this is more efficient on big numbers:
import Data.List (nub)
import Data.Ratio
totient :: (Integral a) => a -> a
totient 1 = 1
totient n = numerator ratio `div` denominator ratio
  where ratio = foldl (\acc x -> acc * (1 - (1 % x)))
                   (n % 1) $ nub (primeFactors n)
-}

-- 35. Find an integer's prime factors in a flat, ascending list  ------------
--     e.g. primeFactors 315 ==> [3,3,5,7]
primeFactors :: Int -> [Int] -- CHEATED
primeFactors n = primeFactors' n 2
  where
    primeFactors' 1 _ = []
    primeFactors' n f
      | f * f > n      = [n]
      | n `rem` f == 0 = f : primeFactors' (n `div` f) f
      | otherwise      = primeFactors' n (f + 1)

-- 36. Determine the prime factors of x, but with "multiplicity"  ------------
--     e.g. primeFactorsMult 315 ==> [(3,2),(5,1),(7,1)]
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = map swap . encode . primeFactors
  where swap (x, y) = (y, x)

-- 37. Calculate totient (improved) ------------------------------------------
phi :: Int -> Int
phi m = undefined -- FUCK THIS UGH

-- 38. FUCK THIS TOO
--
-- 39. A list of prime numbers b/w 2 bounds ----------------------------------
primesR :: Int -> Int -> [Int]
primesR n m = filter isPrime [n..m]

-- 40. Goldbach's conjecture. Sum any number w/ 2 primes ---------------------
goldbach :: Int -> (Int, Int)
goldbach n = head $
  [ (x, y)
  | x <- pr
  , y <- pr
  , x + y == n
  ]
  where pr = primesR 2 (n - 2)

-- 41. Given a range of integers, print a list of evens
--     and their Goldbach compsitions
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList n m = map goldbach $ [even_n, even_n+2 .. m]
  where even_n = max ((n + 1) `div` 2 * 2) 4

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
-- ^ 3rd Int is all cases where both primes > Int
goldbachList' n m x = filter (\(a,b) -> a > x && b > x) $
  goldbachList n m

-- }}}
-- vim: fdm=marker
