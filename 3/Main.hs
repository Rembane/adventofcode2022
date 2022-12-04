{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Debug.Trace
import qualified Data.ByteString.Char8 as B

toPoints :: Char -> Int
toPoints = fromJust . (`M.lookup` charMap)
  where charMap = M.fromList $ zip ['A'..'Z'] [27..] ++ zip ['a'..'z'] [1..]

both :: (a -> b) -> (a, a) -> (b, b)
both f (x1, x2) = (f x1, f x2)

triplets :: [a] -> [[a]]
triplets xs = case splitAt 3 xs of
  ([], [])   -> []
  (xs1, xs2) -> xs1:triplets xs2

main :: IO ()
main = do
  backpacks <- filter (/= B.empty) . B.split '\n' <$> B.readFile "input"
  (putStrLn . ("Solution 1: " <>) . show . sum . concatMap (map toPoints . S.toList . uncurry S.intersection . both (S.fromList . B.unpack) . uncurry B.splitAt . ((`div` 2) . B.length &&& id))) backpacks

  (putStrLn . ("Solution 2: " <>) . show . sum . concatMap (map toPoints . S.toList . foldl1 S.intersection . map (S.fromList . B.unpack)) . triplets) backpacks


