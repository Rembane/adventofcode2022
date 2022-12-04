module Main where

import Control.Arrow
import Data.Char
import Data.List

parseLine :: String -> (Int, Int, Int, Int)
parseLine s = let (i1, s1) = parser s
                  (i2, s2) = parser s1
                  (i3, s3) = parser s2
                  (i4, s4) = parser s3
               in (i1,i2,i3,i4)
  where parser = (read *** drop 1) . span isDigit

main :: IO ()
main = do
  pairs <- map parseLine . lines <$> readFile "input"
  putStrLn . ("Solution 1: " <>) . show . length . filter (\(i1,i2,i3,i4) -> i1 <= i3 && i2 >= i4 || i1 >= i3 && i2 <= i4) $ pairs
  putStrLn . ("Solution 2: " <>) . show . length . filter (\(i1,i2,i3,i4) -> (any (>1) . scanl' (+) 0 . map snd . sortOn fst) [(i1, 1), (i2 + 1, -1), (i3, 1), (i4, -1)]) $ pairs
