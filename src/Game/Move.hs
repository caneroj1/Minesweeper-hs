module Game.Move where

import Text.ParserCombinators.ReadPrec
import Text.ParserCombinators.ReadP
import Data.Char
import Control.Applicative
import Text.Read

data Move = RevealTile (Int, Int) |
            FlagTile   (Int, Int) |
            UnflagTile (Int, Int) |
            None deriving Show

instance Read Move where
  readPrec = lift parseMove
  readsPrec _ = readP_to_S parseMove

parseComma :: ReadP ()
parseComma =
  skipSpaces >>
  char ','   >>
  skipSpaces

parseDigit :: ReadP Int
parseDigit = read <$> munch1 isDigit

parseCoords :: ReadP (Int, Int)
parseCoords = do
  l <- parseDigit
  parseComma
  r <- parseDigit
  return (l, r)

parseReveal :: ReadP Move
parseReveal =
  fmap RevealTile parseCoords

parseFlag :: ReadP Move
parseFlag = do
  _ <- string "flag"
  skipSpaces
  fmap FlagTile parseCoords

parseUnflag :: ReadP Move
parseUnflag = do
  _ <- string "unflag"
  skipSpaces
  fmap UnflagTile parseCoords

parseMove :: ReadP Move
parseMove =
  parseFlag   <|>
  parseUnflag <|>
  parseReveal <|>
  return None
