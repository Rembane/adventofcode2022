module Main where

import Control.Monad ((>=>))
import Data.Char (digitToInt)
import Data.Function
import Data.List
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe
import Data.Semigroup

data Cell = Visible Int | Invisible Int
  deriving Show

instance Semigroup Cell where
  Invisible i1 <> Invisible i2 = Invisible (max i1 i2)
  Visible   i1 <> Invisible i2 = Invisible (max i1 i2)
  Invisible i1 <> Visible   i2 = if i2 > i1 then Visible i2 else Invisible i1
  Visible   i1 <> Visible   i2 = if i2 > i1 then Visible i2 else Invisible i1

isVisible :: Cell -> Bool
isVisible (Visible _) = True
isVisible _           = False

newtype VisibleAtAll = VisibleAtAll {getVisibleAtAll :: Cell}

instance Semigroup VisibleAtAll where
  VisibleAtAll (Invisible _) <> VisibleAtAll (Invisible _) = VisibleAtAll (Invisible (-1))
  _                          <> _                          = VisibleAtAll (Visible (-1))

newtype Board = Board {getBoard :: [[Cell]]}
  deriving Show

instance Semigroup Board where
  b1 <> b2 = Board . map (map getVisibleAtAll) $ (zipWith (zipWith (<>)) `on` (map (map VisibleAtAll) . getBoard)) b1 b2

getVisibility :: [Int] -> [Cell]
getVisibility = scanl1 (<>) . map Visible

somersault :: ([a] -> [b]) -> [[a]] -> [[[b]]]
somersault f xss =
  map
    ($ xss)
    [ go
    , reverseLine . go . reverseLine
    , transpose . go . transpose
    , transpose . reverseLine . go . reverseLine . transpose
    ]
 where go = map f
       reverseLine = map reverse

visibleRepr :: Board -> String
visibleRepr =  intercalate "\n" . map (concatMap go) . getBoard
  where go (Visible i)   = "\ESC[1m" <> show i <> "\ESC[0m"
        go (Invisible i) = show i

main :: IO ()
main = do
  boards <- map Board . somersault getVisibility . map (map digitToInt) . lines <$> readFile "input"
  putStrLn $ intercalate "\n\n" $ map visibleRepr boards

  putStrLn ""
  print . sum . map (length . filter isVisible) . getBoard . sconcat . fromJust . nonEmpty $ boards
