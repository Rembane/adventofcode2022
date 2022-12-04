{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.Map.Strict as M
import Data.Maybe

data RPS = Rock | Paper | Scissors
  deriving (Eq, Ord, Show)

data Outcome = LeftWins | RightWins | Draw
  deriving (Eq, Ord)

data Relation = Relation RPS RPS Outcome

type Me = RPS
type Opponent = RPS

table :: [Relation]
table =
  [ Relation Rock     Rock     Draw
  , Relation Rock     Paper    RightWins
  , Relation Rock     Scissors LeftWins
  , Relation Paper    Rock     LeftWins
  , Relation Paper    Paper    Draw
  , Relation Paper    Scissors RightWins
  , Relation Scissors Rock     RightWins
  , Relation Scissors Paper    LeftWins
  , Relation Scissors Scissors Draw
  ]

firstStrategyGuide :: Opponent -> Me -> Int
firstStrategyGuide opp me =
  let t = M.fromList (map (\(Relation o m r) -> ((o, m), r)) table)
   in scoreRPS me + case fromJust (M.lookup (opp, me) t) of
     LeftWins  -> 0
     Draw      -> 3
     RightWins -> 6

secondStrategyGuide :: Opponent -> Outcome -> Int
secondStrategyGuide opp out =
  let t = M.fromList (map (\(Relation o m r) -> ((o, r), m)) table)
      me = fromJust (M.lookup (opp, out) t)
   in scoreRPS me + case out of
     LeftWins  -> 0
     Draw      -> 3
     RightWins -> 6


parseRPS :: String -> RPS
parseRPS = \case
  "A" -> Rock
  "B" -> Paper
  "C" -> Scissors
  "X" -> Rock
  "Y" -> Paper
  "Z" -> Scissors
  s -> error $ "Huh? " <> s

scoreRPS :: RPS -> Int
scoreRPS = \case
  Rock     -> 1
  Paper    -> 2
  Scissors -> 3

parseWhoWins :: String -> Outcome
parseWhoWins = \case
  "X" -> LeftWins
  "Y" -> Draw
  "Z" -> RightWins

main :: IO ()
main = do
  input <- map words . lines <$> readFile "input"
  putStrLn . ("First part: " <>) . show . sum . map ((\[them, me] -> firstStrategyGuide them me) . map parseRPS) $ input
  putStrLn . ("Seconds part: " <>) . show . sum . map (\[w1,w2] -> secondStrategyGuide (parseRPS w1) (parseWhoWins w2)) $ input
