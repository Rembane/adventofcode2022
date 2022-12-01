module Main where

import Data.List
import Data.Ord

main :: IO ()
main = do
  elves <- foldr go [] . lines <$> readFile "input"
  putStrLn $ "Answer to part 1: " <> show (maximum elves)
  putStrLn $ "Answer to part 2: " <> show (sum . take 3 . sortOn Down $ elves)

go :: String -> [Int] -> [Int]
go ""  xs     = 0:xs
go s (x:xs) = (read s + x):xs
