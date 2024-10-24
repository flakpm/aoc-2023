module Day01 where

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Data.Char (digitToInt, isDigit)
import Data.List (sortBy, tails)
import Data.Ord (comparing)
import Paths_aoc2023 (getDataFileName)

day01 :: IO ()
day01 = do
  inputLines <- lines <$> (getDataFileName "day01-input.txt" >>= readFile)
  putStrLn "Part 1:"
  print $ part1 inputLines
  putStrLn "Part 2:"
  print $ part2 inputLines

part1 :: [String] -> Int
part1 =
  sum
    . map (liftA2 (+) ((10 *) . digitToInt . head) (digitToInt . last))
    . filter (not . null)
    . map (filter isDigit)

part2 :: [String] -> Int
part2 = sum . map digitsFromLine

digitsFromLine :: String -> Int
digitsFromLine =
  liftA2 (+) ((10 *) . head) last
    . map fst
    . sortBy (comparing snd)
    . map (fst &&& (fst . snd))
    . concat
    . flip map numMap
    . matchDigit

matchDigit :: String -> (String, Int) -> [(Int, (Int, String))]
matchDigit =
  liftA2 filter ((. (snd . snd)) . (==) . fst)
    . liftA2 zip (repeat . snd)
    . (zip [0 ..] .)
    . flip (slidingWindow . length . fst)

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n = takeWhile ((n==) . length) . map (take n) . tails

numMap :: [(String, Int)]
numMap =
  [ ("1", 1),
    ("2", 2),
    ("3", 3),
    ("4", 4),
    ("5", 5),
    ("6", 6),
    ("7", 7),
    ("8", 8),
    ("9", 9),
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9)
  ]

