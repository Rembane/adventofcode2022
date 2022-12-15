{-# LANGUAGE TupleSections #-}
module Main where

import Control.Applicative hiding (some)
import Control.Arrow
import Control.Monad ((>=>))
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Debug.Trace
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

data Cell = Rock | Sand
  deriving (Eq, Show)

newtype Point = Point { unPoint :: (Int, Int) }
  deriving (Eq, Ord, Show)

type Parser = Parsec Void String

-- | A sparse cave should be a good fit for a Map especially if we put the
-- columns first and then the rows to easily get a whole row worth of sand.
type Cave = Map Int (Map Int Cell)

intParser :: Parser Int
intParser = read <$> some digitChar

posParser :: Parser Point
posParser = fmap Point ((,) <$> intParser <* char ',' <*> intParser)

lineParser :: Parser [Point]
lineParser = posParser `sepBy` string " -> "

both :: (a -> b) -> (a, a) -> (b, b)
both f = f *** f

tupleTranspose :: ((a, b), (c, d)) -> ((a, c), (b, d))
tupleTranspose ((a, b), (c, d)) = ((a, c), (b, d))

owl :: (c -> d) -> (a -> b -> c) -> a -> b -> d
owl = (.) . (.)

interpolateLine :: [Point] -> [Point]
interpolateLine = concat . uncurry (zipWith ((uncurry (liftA2 (curry Point)) . both (uncurry enumFromTo . (uncurry min &&& uncurry max))) `owl` curry tupleTranspose)) . (id &&& drop 1) . map unPoint

getCell :: Point -> Cave -> Maybe Cell
getCell (Point (x, y)) = M.lookup x >=> M.lookup y

updateCell :: Point -> Cell -> Cave -> Cave
updateCell (Point (x, y)) newCellContent = M.insertWith M.union x (M.singleton y newCellContent)

-- | The sand falls down from 500,0
-- Evaluate to Nothing if the sand falls into the abyss.
fallUntilResting :: Point -> Cave -> Maybe Cave
fallUntilResting (Point (x, y)) cave = M.lookup x cave >>= M.lookupGT y >>= (\(y', _) ->
  case getCell (Point (x-1, y')) cave of
    Nothing -> fallUntilResting (Point (x-1, y')) cave
    Just _  -> case getCell (Point (x+1, y')) cave of
                 Nothing -> fallUntilResting (Point (x+1, y')) cave
                 Just _  -> pure $ updateCell (Point (x, y' - 1)) Sand cave
  )

fallUntilRestingOnInfiniteFloor :: Int -> Point -> Cave -> (Point, Cave)
fallUntilRestingOnInfiniteFloor infiniteFloorLevel (Point (x, y)) cave =
  let result = M.lookup x cave >>= M.lookupGT y >>= (\(y', _) ->
                case getCell (Point (x-1, y')) cave of
                  Nothing -> pure $ fallUntilRestingOnInfiniteFloor infiniteFloorLevel (Point (x-1, y')) cave
                  Just _  -> case getCell (Point (x+1, y')) cave of
                               Nothing -> pure $ fallUntilRestingOnInfiniteFloor infiniteFloorLevel (Point (x+1, y')) cave
                               Just _  -> pure (Point (x, y' - 1), updateCell (Point (x, y' - 1)) Sand cave)
                )
   in fromMaybe (Point (x, infiniteFloorLevel - 1), updateCell (Point (x, infiniteFloorLevel - 1)) Sand (updateCell (Point (x, infiniteFloorLevel)) Rock cave)) result

main :: IO ()
main = do
  cave <- foldr (\p cave -> updateCell p Rock cave) M.empty . concatMap interpolateLine . fromJust . parseMaybe (lineParser `sepBy` newline) <$> readFile "input"

  putStr "Part 1: "
  print . length . unfoldr (fmap (1,) . fallUntilResting (Point (500,0))) $ cave

  let infiniteFloorLevel = (2+) . maximum . concatMap M.keys . M.elems $ cave

  putStr "Part 2: "
  print . length . takeWhile ((/= Point (500,0)) . fst) . iterate (fallUntilRestingOnInfiniteFloor infiniteFloorLevel (Point (500, 0)) . snd) $ (Point (0, 0), cave)
