module Main where

import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Void (Void)
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char

data Packet
  = PInt Int
  | PP [Packet]
  deriving (Eq, Show)

instance Ord Packet where
  compare (PInt i)      (PInt j)      = compare i j
  compare (PP [])       (PP (_:_))    = LT
  compare (PP (_:_))    (PP [])       = GT
  compare (PP (p1:ps1)) (PP (p2:ps2)) = case compare p1 p2 of
                                          EQ -> compare (PP ps1) (PP ps2)
                                          v  -> v
  compare (PP [])       (PP [])       = EQ
  compare pi@(PInt _)   ps2@(PP _)    = compare (PP [pi]) ps2
  compare ps1@(PP _)    pj@(PInt _)   = compare ps1 (PP [pj])

type PacketParser = Parsec Void String [(Packet, Packet)]

packetParser :: PacketParser
packetParser = ((,) <$> (parseOnePacket <* newline) <*> (parseOnePacket <* newline)) `sepBy` newline
  where parseOnePacket = PP <$> between (char '[') (char ']') (choice [PInt . read <$> some digitChar, parseOnePacket] `sepBy` char ',')

main :: IO ()
main = do
  c <- readFile "input"
  (print . sum . map fst . filter snd . zipWith (\idx (p1, p2) -> (idx, p1 <= p2)) [1..] . fromJust . parseMaybe packetParser) c
