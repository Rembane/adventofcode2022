{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Char

parseChar :: Char -> Int
parseChar = \case
  '0' ->  0
  '1' ->  1
  '2' ->  2
  '-' -> -1
  '=' -> -2

toSNAFU :: Int -> String
toSNAFU = reverse . go . reverse . toPositiveFives
  where
    go []     = []
    go (x:xs)
      | x > 2 = let x' = toNegChar x
                 in case xs of
                      []     -> x' : go [1]
                      (y:ys) -> x' : go (y + 1:ys)
      | otherwise = intToDigit x : go xs

    toNegChar 5 = '0'
    toNegChar 4 = '-'
    toNegChar 3 = '='

    toPositiveFives n = go n . reverse . takeWhile (<n) $ iterate (5*) 1
      where
        go _ []     = []
        go n (d:ds) = let (q, r) = divMod n d in q : go r ds

main :: IO ()
main = print . toSNAFU . sum . map (sum . zipWith (\factor c -> factor * parseChar c) (iterate (5*) 1) . reverse) . lines =<< readFile "input"
