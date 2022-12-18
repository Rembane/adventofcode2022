module Main where

import Control.Arrow
import Data.Char
import Data.List
import Data.Set (Set)
import qualified Data.Set as S

type Pos = (Int, Int, Int)

parser :: String -> Pos
parser s = let (x, s1) = (read *** tail) $ span isDigit s
               (y, s2) = (read *** tail) $ span isDigit s1
               (z, _)  = (read *** tail) $ span isDigit s2
            in (x,y,z)

calcNeighbours :: Pos -> [Pos]
calcNeighbours (x, y, z) =
  [(x+1, y, z), (x-1, y, z), (x, y+1, z), (x, y-1, z), (x, y, z+1), (x, y, z-1)]

combinator :: Set Pos -> [Pos] -> Int
combinator allCubes = sum . map ((6 -) . length . filter (`S.member` allCubes) . calcNeighbours)

main :: IO ()
main = do
  positions <- S.fromList . map parser . lines <$> readFile "input"
  print $ combinator positions (S.toList positions)
