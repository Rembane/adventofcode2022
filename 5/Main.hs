{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Data.List
import Data.Maybe
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Parser = Parsec Void String

newtype Crate = Crate { unCrate :: Char }
  deriving Show

data Move = Move
  { quantity    :: Int
  , source      :: Int
  , destination :: Int
  }
  deriving Show

crateParser :: Parser (Maybe Crate)
crateParser = (Just . Crate <$> (char '[' *> upperChar <* char ']')) <|> (Nothing <$ string "   ")

lineOfCratesParser :: Parser [Maybe Crate]
lineOfCratesParser = crateParser `sepBy` char ' '

allCratesParser :: Parser [[Maybe Crate]]
allCratesParser = lineOfCratesParser `sepBy` newline

indicesParser :: Parser [Int]
indicesParser = hspace *> fmap read (some digitChar) `sepBy` some (char ' ')

moveParser :: Parser Move
moveParser = Move
  <$> (string "move " *> fmap read (some digitChar))
  <*> (string " from " *> fmap read (some digitChar))
  <*> (string " to " *> fmap read (some digitChar))

mover :: Move -> Map Int [Crate] -> Map Int [Crate]
mover Move {quantity, source, destination} m =
  let (top, rest) = splitAt quantity (m M.! source)
   in M.adjust (reverse top ++) destination (M.insert source rest m)

main :: IO ()
main = do
  fileContents <- readFile "input"
  let result = runParser (do
                crates         <- allCratesParser
                numberOfStacks <- length <$> indicesParser
                -- Yeah...
                hspace
                newline
                newline
                moves <- moveParser `sepEndBy1` newline

                pure (M.fromList $ zip [1..] $ map catMaybes $ transpose $ map (\stack -> take numberOfStacks (stack ++ repeat Nothing)) (filter (not . null) crates), moves)
                )
                "best input"
                fileContents
  case result of
    Left  e               -> error (show e)
    Right (stacks, moves) -> putStrLn ("Answer part 1: " ++ show (map (unCrate . head . snd) $ M.toAscList $ foldl' (flip mover) stacks moves))
