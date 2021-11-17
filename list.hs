{-- Things that i learn
If the type signature ends with (-> [a]), prob i can use
f [] = []
f (x:xs) = x : f xs
--}
import Data.List (tails)

f :: (a -> b) -> [a] -> [b]
f _ [] = []
f g (x : xs) = g x : f g xs

-- 1. Find the last element of a list
myLast :: [a] -> Maybe a
myLast lst = case lst of
  [] -> Nothing
  [x] -> Just x
  x : xs -> myLast xs

-- 2. Find the last but one element of a list.
myButLast :: [a] -> Maybe a
myButLast = myLast . init

-- 3. Find the K'th element of a list. The first element in the list is number 1.
elementAt :: Int -> [b] -> Maybe b
elementAt _ [] = Nothing
elementAt k (x : xs)
  | k == 1 = Just x
  | otherwise = elementAt (k - 1) xs

-- 4. Find the number of elements of a list.
myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

-- 5. Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

-- Tail Recursive
myTReverse :: [a] -> [a]
myTReverse lst = innerRec [] lst
  where
    innerRec acc lst = case lst of
      [] -> acc
      (x : xs) -> innerRec (x : acc) xs

-- 6. Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: Eq a => [a] -> Bool
isPalindrome lst = lst == myTReverse lst

-- 7. Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten = reverse . innerRec []
  where
    innerRec acc lst =
      case lst of
        List [] -> acc
        Elem x -> x : acc
        List (x : xs) -> innerRec (innerRec acc x) (List xs)

-- 8. Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x : xs) = x : compress (dropWhile (== x) xs)

-- 9. Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x : xs) =
  let (fst, tail) = span (== x) xs
   in (x : fst) : pack tail

-- 10. Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

encode :: Eq a => [a] -> [(Int, a)]
encode = f . pack
  where
    f lst = case lst of
      [] -> []
      x : xs -> (myLength x, head x) : f xs

data EncodeValue a = Multiple Int a | Single a
  deriving (Show)

-- 11. Modified run-length encoding.
encodeModified :: Eq a => [a] -> [EncodeValue a]
encodeModified = map f . encode
  where
    f (n, v)
      | n == 1 = Single v
      | otherwise = Multiple n v

-- 12. Decode a run-length encoded list.
decodeModified :: [EncodeValue a] -> [a]
decodeModified = concatMap f
  where
    f (Single x) = [x]
    f (Multiple n x) = replicate n x

-- 13. Run-length encoding of a list (direct solution).
encodeDirect :: Eq a => [a] -> [EncodeValue a]
encodeDirect [] = []
encodeDirect (x : xs)
  | count == 1 = Single x : encodeDirect xs
  | otherwise = Multiple count x : encodeDirect rest
  where
    (equal, rest) = span (== x) xs
    count = 1 + myLength equal

-- 14. Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = x : x : dupli xs

-- 15. Replicate the elements of a list a given number of times.
repli :: Int -> [a] -> [a]
repli n = concatMap (replicate n)

-- 16. Drop every N'th element from a list.
dropEvery :: Ord a => [a] -> Int -> [a]
dropEvery xs n
  | myLength xs < n = xs
  | otherwise = take (n - 1) xs ++ dropEvery (drop n xs) n

-- 17. Split a list into two parts; the length of the first part is given.
split :: Ord a => Int -> [a] -> ([a], [a])
split n xs = (take n xs, drop n xs)

-- 18. Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included).
-- Start counting the elements with 1.
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x : xs) i j
  | j < 1 = []
  | i > 1 = slice xs (i - 1) (j - 1)
  | otherwise = x : slice xs (i -1) (j -1)

-- 19. Rotate a list N places to the left.
-- mim não ser cocudo

-- 20. Remove the K'th element from a list.
removeAt :: Ord a => Int -> [a] -> ([a], [a])
removeAt _ [] = ([], [])
removeAt n lst =
  let (first, last) = split n lst
   in let (fst, removed) = split (length first - 1) first
       in (removed, fst ++ last)

-- 21. Insert at K'th element of a list
insertAt :: Ord a => a -> [a] -> Int -> [a]
insertAt _ [] _ = []
insertAt x lst 1 = x : lst
insertAt x (y : ys) n = y : insertAt x ys (n - 1)

-- 22. Range
range :: Enum a => a -> a -> [a]
range x y = [x .. y]

-- 26. Generate the combinations of K distinct objects chosen from the N elements of a list
-- use tails
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n lst = do
  x : xs <- tails lst
  ys <- combinations (n - 1) xs
  return (x : ys)
