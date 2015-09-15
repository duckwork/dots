import Data.List (group)
-- 01-10 {{{
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
encode' :: Eq a => [a] -> [(Int, a)]
encode' = map (\x -> (length x, head x)) . group
-- }}}
-- 11-20 {{{
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
removeAt' n (x;xs) = (l , x:r)
  where (l, r) = removeAt (n - 1) xs
-- OR WITH MAYBE
removeAtMaybe :: Int -> [a] -> (Maybe a, [a])
removeAtMaybe _ [] = (Nothing, [])
removeAtMaybe 1 (x:xs) = (Just x, xs)
removeAtMaybe k (x:xs) = let (a, r) = removeAt (k - 1) xs
                          in (a, x:r)
-- }}}
-- vim: fdm=marker
