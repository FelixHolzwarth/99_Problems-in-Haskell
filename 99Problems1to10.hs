-- p01 find last element of a list
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast l@(_:_) = Just lastElem
  where
    lastElem = head $ reverse l

myLast' :: [a] -> Maybe a
myLast' []      = Nothing
myLast' (x:[])  = Just x
myLast' (_:xs)  = myLast' xs

-- p02 find the last but one element of a list
myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast (x:_:[]) = Just x
myButLast l@(_:_) = Just getLastButOne
  where
    getLastButOne = head $ drop 1 $ reverse l

myButLast' :: [a] -> Maybe a
myButLast' []         = Nothing
myButLast' (_:[])     = Nothing
myButLast' (x:_:[])  = Just x
myButLast' (_:xs)         = myButLast' xs

-- p03 find the k'th element of a list. The first element of a list is number 1
elementAt :: Int -> [a] -> Maybe a
elementAt 0 _ = Nothing
elementAt _ [] = Nothing
elementAt index lst = case drop (index - 1) lst of
                        []        -> Nothing
                        otherwise -> Just $ head $ drop (index - 1) lst

elementAt' :: Int -> [a] -> Maybe a
elementAt' 0 _          = Nothing
elementAt' 1 []         = Nothing
elementAt' 1 (x:_)     = Just x
elementAt' index (_:xs) = elementAt' (index - 1) xs


-- p04 find the number of elements in a list
myLength :: [a] ->  Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' = foldl (\acc _ -> acc + 1) 0


-- p05 reverse a list
myReverse :: [a] -> [a]
myReverse []          = []
myReverse lst = reverse' lst []
    where
      reverse' (x:xs) acc = reverse' xs (x:acc)
      reverse' [] acc     = acc

myReverse' :: [a] -> [a]
myReverse' lst = foldl (\acc x -> x:acc) [] lst

myReverse'' :: [a] -> [a]
myReverse'' lst = foldr (\x acc -> acc ++ [x]) [] lst


-- p06 find out wether a list is a palindrom. A Palindrom can be read forward and backward
isPalindrom :: (Eq a) => [a] -> Bool
isPalindrom lst = isPalindrom' lst (reverse lst)
  where
    isPalindrom' [] _            = True
    isPalindrom' _ []            = True
    isPalindrom' (x:xs) (y:ys)   = x == y && isPalindrom' xs ys

isPalindrom' :: (Eq a) => [a] -> Bool
isPalindrom' lst = lst == (reverse lst)

-- p07 new Data-Type for NestedList
data NestedList a = Elem a | List [NestedList a]
  deriving Show

flatten :: NestedList a -> [a]
flatten (List [])            = []
flatten (Elem a)             = [a]
flatten (List ((Elem a):xs)) = a:flatten (List xs)
flatten (List (as@(List _):xs)) = (flatten as) ++ flatten (List xs)


-- p08 eliminate consecutive duplicates of list elements
compress :: (Eq a) => [a] -> [a]
compress []           = []
compress [x]          = [x]
compress (y:ys)       = reverse $ compressHelper ys [y]
  where
      compressHelper (x:xs) acc = if x /= head acc
                                then compressHelper xs (x:acc)
                                else compressHelper xs acc
      compressHelper [] acc     = acc

compress' :: (Eq a) => [a] -> [a]
compress' []        = []
compress' [x]       = [x]
compress' (x:xs)    = x:compress (dropWhile (== x) xs)


-- p09 pack consecutive duplicates of list elements into sublists.
pack :: (Eq a) => [a] -> [[a]]
pack x = zipWith replicate nums elems
  where
    nums  = map snd $ countConsecutiveElems x
    elems = map fst $ countConsecutiveElems x

countConsecutiveElems :: (Eq a) => [a] -> [(a,Int)]
countConsecutiveElems []      = []
countConsecutiveElems [z]     = [(z,1)]
countConsecutiveElems (z:zs)  = reverse $ helper zs [(z,1)]
  where
    helper []     acc         = acc
    helper (x:_) acc@(y:ys)  = if x == (fst y)
                            then modify y:ys
                            else (x,1):acc
    modify ::(a,Int) -> (a,Int)
    modify = (\(e,num) -> (e,num + 1))

-- p10 Run-length encoding of a list. Use the result of problem P09
-- to implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) where N is
-- the number of duplicates of the element E.
encode :: (Eq a) =>  [a] -> [(Int,a)]
encode []         = []
encode lst@(_:_)  = map (\x -> (length x,head x)) $ pack $ lst
