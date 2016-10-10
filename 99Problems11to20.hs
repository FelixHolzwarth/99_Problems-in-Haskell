-- pack consecutive duplicates of list elements into sublists.
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


encode :: (Eq a) =>  [a] -> [(Int,a)]
encode []         = []
encode lst@(_:_)  = map (\x -> (length x,head x)) $ pack $ lst

-- p11 Modified run lenght encoding , modify the result of problem 10 in such way that if
-- an element has no duplicates it is simply copied into the result list. Only elements
-- with duplicates are transferred as (N E) lists

data Encoding a = Single a | Multiple Int a

encodeModified :: (Eq a) => [a] -> [Encoding a]
encodeModified []  = []
encodeModified [x] = [Single x]
encodeModified x   =  map (\(a,e) -> if a == 1
                                          then Single e
                                          else Multiple a e ) $ encode $ x

-- p12 Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11.
-- Construct its uncompressed version.

decodeModified :: [Encoding a] -> [a]
decodeModified []         = []
decodeModified lst@(_:_) =  concatMap (\e -> case e of
                                        (Single a)       -> replicate 1 a
                                        (Multiple num a) -> replicate num a) lst


-- p14 Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x:x:dupli xs

-- p15 Replicate the elements of a list a given number of times.

repli :: [a] -> Int -> [a]
repli [] _         = []
repli lst n  = concatMap (\e -> replicate n e) lst

-- p16 Drop every N'th element from a list.

drop :: [a] -> Int -> [a]
drop [] _         = []
drop lst n  = helper n lst
  where
    helper 0  (y:ys) = helper n ys
    helper n2 lst    = helper (n2 - 1) lst

drop' :: [a] -> Int -> [a]
drop' [] _        = []
drop' lst n = map fst $ filter ((== 0) . (`mod` n)  . snd) $ zip lst [1..]
