-- p21 Insert an element at a given position into a list.

insertAt :: a -> [a] -> Int -> [a]
insertAt e lst n = cut1 ++ [e] ++ cut2
  where
    (cut1,cut2) = splitAt n lst

-- p22 Create a list containing all integers within a given range.

range :: Int -> Int -> [Int]
range start end = [start.. end]    
