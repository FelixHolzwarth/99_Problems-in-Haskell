import System.Random
import Data.List

-- p21 Insert an element at a given position into a list.

insertAt :: a -> [a] -> Int -> [a]
insertAt e lst n = cut1 ++ [e] ++ cut2
  where
    (cut1,cut2) = splitAt n lst

-- p22 Create a list containing all integers within a given range.

range :: Enum a => a -> a -> [a]
range start end = [start.. end]

-- 23 Extract a given number of randomly selected elements from a list.

rndSelect :: Int -> [a] -> IO [a]
rndSelect n lst = newStdGen >>= return . map (lst !!) . take n . randomRs (0,sizeOfList)
  where
    sizeOfList = (length lst) - 1

{-rndSelectDistinct :: Int -> [a] -> Maybe (IO [a])
rndSelectDistinct n lst | n <= sizeOfList = do
  gen <- newStdGen
  (return . map (lst !!) . take n . nub . randomRs (0,sizeOfList)) gen
                        | otherwise       = Nothing
    where
        sizeOfList = (length lst) - 1-}

-- p24 Draw N different random numbers from the set 1..M.

diffSelect :: Int -> Int -> IO [Int]
diffSelect n upper = newStdGen >>= return . take n . nub . randomRs (1,upper)

-- p25 Generate a random permutation of the elements of a list.

rndPermutation :: [a] -> IO [a]
rndPermutation lst = newStdGen >>= return . map (lst !!) . take lstSize . nub . randomRs (0,(lstSize - 1))
  where
    lstSize = (length lst)


combinations :: Int -> [a] -> [[a]]
combinations n lst
  where
    len = length lst - 1
    combOfKOutOfN :: [[Int]]
    combOfKOutOfN = cycle 
