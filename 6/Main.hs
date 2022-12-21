module Main where

import Data.Char
import Data.List
import Data.Maybe

chunker :: String -> [String]
chunker (c1:c2:c3:c4:s) = [c1,c2,c3,c4] : chunker (c2:c3:c4:s)
chunker _               = []

chunker2 :: String -> [String]
chunker2 s = case splitAt 14 s of
               (prefix, rest) | length prefix == 14 -> prefix : chunker2 (tail prefix ++ rest)
                              | otherwise           -> []

main :: IO ()
main = do
  contents <- readFile "input"
  putStrLn $ "Solution 1: " ++ (show . head . catMaybes . zipWith (\i s -> if s == nub s then Just (i, s) else Nothing) [4,5..] . chunker . filter isLower) contents
  putStrLn $ "Solution 2: " ++ (show . head . catMaybes . zipWith (\i s -> if s == nub s then Just (i, s) else Nothing) [14,15..] . chunker2 . filter isLower) contents
