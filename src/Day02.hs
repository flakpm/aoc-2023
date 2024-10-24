module Day02 where

import Control.Arrow ((&&&))
-- import Control.Monad (ap)
-- import Data.Bool (bool)
import Data.Char (isDigit)
import Paths_aoc2023 (getDataFileName)

day02 :: IO ()
day02 = do
  inputLines <- lines <$> (getDataFileName "day02-input.txt" >>= readFile)
  putStrLn "Part 1:"
  print $ part1 inputLines

part1 :: [String] -> Int
part1 = sum . readStringList . map fst . filter (validateAllGames . snd) . splitAllGames

splitAllGames :: [String] -> [(String, [String])]
splitAllGames = map splitSingleGame

splitSingleGame :: String -> (String, [String])
splitSingleGame = (head &&& splitOn ';' . last) . splitOn ':' . drop 4 . filter (/= ' ')

validateAllGames :: [String] -> Bool
validateAllGames = all validateGame

validateGame :: String -> Bool
validateGame = validateSet . map (takeWhile isDigit &&& dropWhile isDigit) . splitOn ','

validateSet :: [(String, String)] -> Bool
validateSet = flip all cubeMap . validateSingleColor

validateSingleColor :: [(String, String)] -> (String, Int) -> Bool
validateSingleColor x y = (snd y >=) . sum $ (readStringList (map fst (filter ((fst y ==) . snd) x)))

readStringList :: [String] -> [Int]
readStringList xs = if null xs then [0] else map read xs

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s = takeWhile (/= c) s : splitOn c (if null (dropWhile (/= c) s) then [] else tail (dropWhile (/= c) s))

cubeMap :: [(String, Int)]
cubeMap = [("red", 12), ("green", 13), ("blue", 14)]
