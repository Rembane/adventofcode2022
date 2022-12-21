module Main where

import Data.Char
import Data.List
import Data.Maybe

chunker :: String -> [String]
chunker (c1:c2:c3:c4:s) = [c1,c2,c3,c4] : chunker (c2:c3:c4:s)
chunker _               = []

main :: IO ()
main = print . head . catMaybes . zipWith (\i s -> if s == nub s then Just (i, s) else Nothing) [4,5..] . chunker . filter isLower =<< readFile "input"
