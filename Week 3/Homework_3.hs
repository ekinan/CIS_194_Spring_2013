module Homework_3 where

-- Exercise 1 solution

skips :: [a] -> [[a]]
skips lst = map allNthElements seqToN
 where
   seqToN = [1..(length lst)]
   indexElemPair = zip seqToN lst
   
   isNthElement n (i, _) = (i `mod` n == 0) 
   allNthElements n = map snd (filter (isNthElement n) indexElemPair)
   
-- Exercise 2 solution

localMaxima :: [Integer] -> [Integer]
localMaxima ls = map sndVal (filter isLocalMaxima prevElemNextLst)
  where
   isLocalMaxima (x, y, z) = y > x && y > z
   sndVal (x, y, z) = y
   
   size = length ls
   prev = (take 1 ls) ++ (take (size - 1) ls)
   next = (drop 1 ls) ++ (drop (size - 1) ls)
   prevElemNextLst = zip3 prev ls next
   
-- Exercise 3 solution

countFreq :: [Integer] -> Integer -> Int
countFreq ls x = length (filter (== x) ls)

drawBar :: Int -> Int -> [String]
drawBar maxFreq freq = (replicate (maxFreq - freq) " ") ++ (replicate freq "*")

combineBarPoint :: String -> String -> String
combineBarPoint barPointOne barPointTwo = barPointOne ++ barPointTwo

combineListOfBars :: [String] -> [String] -> [String]
combineListOfBars barListOne barListTwo = zipWith combineBarPoint barListOne barListTwo

histogram :: [Integer] -> String
histogram ls = unlines histogramLines
  where
	seqTo9 = [0..9]
	freqList = map (countFreq ls) seqTo9
	maxFreq = maximum freqList
	barList = map (drawBar maxFreq) freqList
	barLines = foldl combineListOfBars (replicate maxFreq "") barList
	seqTo9AsString = concat (map (\x -> show x) seqTo9)
	axisLines = [(replicate (length seqTo9) '='), seqTo9AsString]
	histogramLines = barLines ++ axisLines
	
