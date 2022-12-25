module Main where

import Control.Arrow ((&&&), (***))
import Data.List
import Data.Maybe
import Data.Semigroup
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void (Void)

type Pos = (Int, Int)

north :: Pos -> (Pos, [Pos])
north (x, y) = ((x, y-1), [x-1, x, x+1] `zip` repeat (y-1))

south :: Pos -> (Pos, [Pos])
south (x, y) = ((x, y+1), [x-1, x, x+1] `zip` repeat (y+1))

west :: Pos -> (Pos, [Pos])
west (x, y) = ((x-1, y), repeat (x-1) `zip` [y-1, y, y+1])

east :: Pos -> (Pos, [Pos])
east (x, y) = ((x+1, y), repeat (x+1) `zip` [y-1, y, y+1])


newPositionsForAllElves :: Set Pos -> [Pos -> (Pos, [Pos])] -> Set Pos
newPositionsForAllElves s fs = (uncurry (flip S.union) . (S.fromList *** (S.difference s . S.fromList . concat)) . unzip . filter ((==1) . length . snd) . M.toList . M.fromListWith (++) . mapMaybe newPositionForOneElf . S.toList) s
  where
    newPositionForOneElf p = case filter (all (`S.notMember` s) . snd) (map ($ p) fs) of
                               []                  -> Nothing -- All neighbours
                               ps | 4 == length ps -> Nothing -- No  neighbours
                                  | otherwise      -> Just (fst (head ps), [p])


loop :: Set Pos -> [Pos -> (Pos, [Pos])] -> [Set Pos]
loop s fs@(f:fs') = s : loop (newPositionsForAllElves s fs) (fs' ++ [f])

both :: (a -> b) -> (a, a) -> (b, b)
both f = f *** f

toGrid :: Set Pos -> String
toGrid s =
  intercalate
    "\n"
    (map (\y -> map (\x -> if S.member (x,y) s then '#' else '.') [xMin..xMax]) [yMin..yMax])
  where ((xMin, xMax), (yMin, yMax)) = (both ((getMin *** getMax) . foldMap (Min &&& Max)) . unzip . S.toList) s

main :: IO ()
main = do
  elves <- S.fromList . concat . zipWith (\y -> catMaybes . zipWith (\x c -> if c == '#' then Just (x,y) else Nothing) [0..]) [0..] . lines <$> readFile "input"

  let elves' = loop elves [north, south, west, east] !! 10
  let ((xMin, xMax), (yMin, yMax)) = (both ((getMin *** getMax) . foldMap (Min &&& Max)) . unzip . S.toList) elves'
  print (((succ xMax - xMin) * (succ yMax - yMin)) - S.size elves')
  -- putStrLn $ toGrid $ loop elves [north, south, west, east] !! 10
