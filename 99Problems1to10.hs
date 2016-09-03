-- find last element of a list
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast l@(_:_) = Just lastElem
  where
    lastElem = head $ reverse l

myLast' :: [a] -> Maybe a
myLast' []      = Nothing
myLast' (x:[])  = Just x
myLast' (_:xs)  = myLast' xs

-- find the last but one element of a list
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

--find the k'th element of a list. The first element of a list is number 1
elementAt :: Int -> [a] -> Maybe a
elementAt 0 _ = Nothing
elementAt index [] = Nothing
elementAt index lst = case drop (index - 1) lst of
                        []        -> Nothing
                        otherwise -> Just $ head $ drop (index - 1) lst

elementAt' :: Int -> [a] -> Maybe a
elementAt' 0 _          = Nothing
elementAt' 1 []         = Nothing
elementAt' 1 (x:_)     = Just x
elementAt' index (_:xs) = elementAt' (index - 1) xs


--find the number of elements in a list
myLength :: [a] ->  Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs


--reverse a list
myReverse :: [a] -> [a]
myReverse []          = []
myReverse lst = reverse' lst []
    where
      reverse' (x:xs) acc = reverse' xs (x:acc)
      reverse' [] acc     = acc

myReverse' :: [a] -> [a]
myReverse' lst = foldl (\x xs -> xs:x) [] lst

myReverse'' :: [a] -> [a]
myReverse'' lst = foldr (\x xs -> xs ++ [x]) [] lst
