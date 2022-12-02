{-# LANGUAGE LambdaCase #-}
module Main where

data RPS = Rock | Paper | Scissors
  deriving (Eq, Show)

data LoseDrawWin = Lose | Draw | Win
  deriving Show

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

parseLoseDrawWin :: String -> LoseDrawWin
parseLoseDrawWin = \case
  "X" -> Lose
  "Y" -> Draw
  "Z" -> Win

-- | First player is me, the result is how many points I would get.
playRound :: RPS -> RPS -> Int
playRound Rock  Paper    = 0 + scoreRPS Rock
playRound Rock  Scissors = 6 + scoreRPS Rock
playRound Paper Rock     = 6 + scoreRPS Paper
playRound Paper Scissors = 0 + scoreRPS Paper
playRound Scissors Rock  = 0 + scoreRPS Scissors
playRound Scissors Paper = 6 + scoreRPS Scissors
playRound me opponent
  | me == opponent = 3 + scoreRPS me

playSneakily :: RPS -> LoseDrawWin -> RPS
playSneakily Rock     Lose = Paper
playSneakily Rock     Win  = Scissors
playSneakily Paper    Lose = Rock
playSneakily Paper    Win  = Scissors
playSneakily Scissors Lose = Paper
playSneakily Scissors Win  = Rock
playSneakily rps      Draw = rps

main :: IO ()
main = do
  input <- map words . lines <$> readFile "input2"
  putStrLn . ("First part: " <>) . show . sum . map ((\[them, me] -> playRound me them) . map parseRPS)   $ input
  putStrLn . ("Second part: " <>) . show . sum . map (\[them, guide] -> let theirPlay = parseRPS them in playRound (playSneakily theirPlay (parseLoseDrawWin guide)) theirPlay) $ input
  -- putStrLn . ("Second part: " <>) . show . map (\[them, guide] -> let theirPlay = parseRPS them in (theirPlay, parseLoseDrawWin guide, playSneakily theirPlay (parseLoseDrawWin guide))) $ input

