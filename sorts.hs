import System.IO
import Text.Printf
import Control.Exception
import System.CPUTime
import Data.Time.Clock
--

-- selection :: (Ord a) => [a] -> [a]
-- selection [] = []
-- selection xs = (min' xs) : selection (remove (min' xs) xs)

-- min' :: (Ord a) => [a] -> a
-- min' [] = []
-- min' [x] = x
-- min' (x:xs) | x <= (min' xs) = x
--             | otherwise = min' xs

-- remove :: (Ord a) => a -> [a] -> [a]
-- remove a [] = []
-- remove a (x:xs) | a == x = xs
--                 | otherwise = x : (remove a xs)

--

insertion :: (Ord a) => [a] -> [a]
insertion [] = []
insertion (x:xs) = insertOrd x (insertion xs)

insertOrd ::(Ord a) => a -> [a] -> [a]
insertOrd x [] = [x]
insertOrd x (y:ys) | x <= y = (x:y:ys)
                   | otherwise = y: (insertOrd x ys)
                   
--

bubble :: (Ord a) => [a] -> [a]
bubble [] = []
bubble x = bubbleOrd x (myLen x)

myLen :: [a] -> Int
myLen [] = 0
myLen (x:xs) = 1 + myLen xs

trade :: (Ord a) => [a] -> [a]
trade [x] = [x]
trade (x:y:zs) | x > y = y : trade (x:zs)
               | otherwise = x : trade (y:zs)

bubbleOrd :: (Ord a) => [a] -> Int -> [a]
bubbleOrd x 0 = x
bubbleOrd x n = bubbleOrd (trade x) (n-1)

-- 


bubbleSort a = do  
    let list = []
    handle <- openFile a ReadMode
    contents <- hGetContents handle
    let singlewords = words contents
        list = f singlewords
        datai = bubble list
    print datai
    hClose handle

-- selectionSort a = do  
--     let list = []
--     handle <- openFile a ReadMode
--     contents <- hGetContents handle
--     let singlewords = words contents
--         list = f singlewords
--         datai = selection list
--     print datai
--     hClose handle
   
insertionSort a = do  
    let list = []
    handle <- openFile a ReadMode
    contents <- hGetContents handle
    let singlewords = words contents
        list = f singlewords
        datai = insertion list
    print datai
    hClose handle
 
f :: [String] -> [Integer]
f = map read

getNumbers :: String -> [Int]
getNumbers str = map read $ words str :: [Int]

time a = do
    start  <- getCPUTime
    v <- a
    end <- getCPUTime
    let diff = (fromIntegral (end-start)/(10^12))
    printf "Tempo de ordenacao: %0.2f ms\n" ((diff :: Double)*1000)
    return v               

-- para ler é apenas: readFile "arquivo.txt"
