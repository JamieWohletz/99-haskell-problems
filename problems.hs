-- 99 Haskell Problems: https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems
-- problem 1
myLast :: [a] -> a
myLast [] = error "An empty list has no last element!"
myLast [x] = x
myLast (_:xs) = myLast xs

-- problem 2
myButLast :: [a] -> a
myButLast [] = error "An empty list has no second to last element!"
myButLast [x] = error "A single-value list has no second to last element!"
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

myButLast' :: [a] -> a
myButLast' = last . init

--problem 3
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n - 1)

--problem 4
myLength :: [a] -> Int
myLength = foldr (\_ -> (+) 1) 0

myLength' :: [a] -> Int
myLength' = foldr (\_ n -> n + 1) 0

--problem 5
myReverse :: [a] -> [a]
myReverse [] = []
--note that it is possible for xs to be an empty list
myReverse (x:xs) = myReverse xs ++ [x]

--problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome phrase = myReverse phrase == phrase

--problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List l) = foldl (\list el -> list ++ flatten el) [] l

--problem 8
compress :: (Eq a) => [a] -> [a]
compress = foldl setAdd []
  where
    setAdd set item
      | null set || last set /= item = set ++ [item]
      | otherwise = set

--problem 9
pack :: (Eq a) => [a] -> [[a]]
pack = foldl listAdd [[]]
  where
    listAdd list item
      | null (last list) || last (last list) == item = init list ++ [last list ++ [item]]
      | otherwise = list ++ [[item]]

--problem 10
encode :: (Eq a) => [a] -> [(Int,a)]
encode [] = []
encode l = map (\sl -> (length sl, head sl)) (pack l)

--problem 11
data Compressed a = Single a | Multiple Int a deriving (Show)
encodeModified :: (Eq a) => [a] -> [Compressed a]
encodeModified l = map squish (encode l)
  where 
    squish (1,c) = Single c
    squish (n,c) = Multiple n c

--problem 12
decodeModified :: [Compressed a] -> [a]
decodeModified [] = []
decodeModified (Single a:xs) = a : decodeModified xs
decodeModified (Multiple n a:xs) = replicate n a ++ decodeModified xs 

--problem 13
encodeDirect :: (Eq a) => [a] -> [Compressed a]
encodeDirect [] = []
encodeDirect (x:xs) = map squish (rCompress [(1, x)] xs)
  where 
    rCompress l [] = l
    rCompress l (y:ys) 
      | x == y = rCompress (xs ++ [(n+1,x)]) ys 
      | otherwise = rCompress (l ++ [(1,y)]) ys
      where 
        n = fst $ last l
        x = snd $ last l
        xs = init l 
    squish (1,a) = Single a
    squish (n,a) = Multiple n a 

--problem 14
dupli :: [a] -> [a]
dupli = foldl (\l x -> l ++ [x,x]) []
--or
dupli' :: [a] -> [a]
dupli' [] = []
dupli' (x:xs) = x:x:dupli' xs

--problem 15
repli :: [a] -> Int -> [a]
repli xs n = foldl (\l x -> l ++ replicate n x) [] xs
--or
repli' :: [a] -> Int -> [a]
repli' [] _ = []
repli' (x:xs) n = replicate n x ++ repli' xs n

--problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = foldl modDrop [] $ zip [1..] xs
  where 
    modDrop l (i,x)
      | i `mod` n == 0 = l 
      | otherwise = l ++ [x]

--problem 17
split :: [a] -> Int -> ([a],[a])
split list splitIndex = split' list splitIndex 1 ([],[])
  where
    split' [] _ _ tuples = tuples
    split' (x:xs) i j (ys,zs)
      | j > i = split' xs i j (ys,zs ++ [x])
      | otherwise = split' xs i (j+1) (ys ++ [x],zs) 

--problem 18
indexMap :: [a] -> [(Int,a)]
indexMap = zip [1..]

slice :: [a] -> Int -> Int -> [a]
slice l begin end = foldl foldHelper [] (indexMap l)
  where
    foldHelper xs (i,x) 
      | i >= begin && i <= end = xs ++ [x]
      | otherwise = xs

    