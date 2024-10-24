module Day02 where

import Control.Arrow ((&&&))
import Control.Monad (ap, liftM2)
import Data.Bool (bool)
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
splitSingleGame = (head &&& splitOn ';' . last) . splitOn ':' . drop 4 . filter (' ' /=)

validateAllGames :: [String] -> Bool
validateAllGames = all validateGame

validateGame :: String -> Bool
validateGame = validateSet . map (takeWhile isDigit &&& dropWhile isDigit) . splitOn ','

validateSet :: [(String, String)] -> Bool
validateSet = flip all cubeMap . flip validateSingleColor

validateSingleColor :: (String, Int) -> [(String, String)] -> Bool
validateSingleColor x = (snd x >=) . sum . readStringList . map fst . filter ((fst x ==) . snd)

readStringList :: [String] -> [Int]
readStringList = ap (bool [0] . (map read)) (not . null)

splitOn :: Char -> String -> [String]
splitOn = (`ap` (not . null)) . (bool [] .) . (liftM2 (:) (uncurry $ takeWhile . (/=)) (uncurry $ liftM2 (.) splitOn ((ap (bool [] . tail) (not . null) .) . dropWhile . (/=))) .) . (,)



cubeMap :: [(String, Int)]
cubeMap = [("red", 12), ("green", 13), ("blue", 14)]
