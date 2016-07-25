{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Game.Board
(
  emptyBoard
, board
, getTiles
, getBounds
, tileAt
, updateAt
, clear
, revealAllMines
, BoardAssoc
, BoardIdx
, Bounds
, Board(..)
, TwoDArray
)
where

import Data.Array
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Game.Tile

type BoardIdx   = (Int, Int)
type BoardAssoc = (BoardIdx, Tile)
type Bounds     = (BoardIdx, BoardIdx)
type TwoDArray  = Array BoardIdx Tile

data Board = Board
  {
    rows  :: Int
  , cols  :: Int
  , tiles :: TwoDArray
  }

revealAllMines :: Board -> Board
revealAllMines b@Board{..} =
  b{ tiles = revealedArray}
  where
    revealedBoard =
      map (\case
              Mine -> RevealedMine
              tile -> tile) $ elems tiles
    revealedArray = listArray (bounds tiles) revealedBoard

clear :: Board -> Bool
clear Board{..} = not . any
  (\case
      RevealedMine -> False
      Mine         -> False
      Revealed _   -> False
      Hidden _     -> True
  ) $ elems tiles

tileAt :: Board -> BoardIdx -> Tile
tileAt Board{..} i = tiles ! i

updateAt :: Board -> BoardIdx -> Tile -> Board
updateAt board@Board{..} i t =
  board { tiles = tiles // [(i, t)]}

getTiles :: Board -> [BoardAssoc]
getTiles Board{..} = assocs tiles

getBounds :: Board -> Bounds
getBounds Board{..} = bounds tiles

emptyBoard :: Int -> Int -> Board
emptyBoard rows cols = Board rows cols tileArray
  where tileArray = toTileArray rows cols $ repeat (Hidden 0)

board :: Int -> Int -> [Tile] -> Board
board rows cols tiles = Board rows cols $ toTileArray rows cols tiles

toTileArray :: Int -> Int -> [Tile] -> TwoDArray
toTileArray rows cols = listArray ((0, 0), (rows - 1, cols - 1))

instance Show Board where
  show Board{..} = border ++ "\n" ++ pass3 ++ border
    where
      printedRows   = chunksOf cols . map show $ elems tiles
      separateCells = intercalate "|"
      pass1         = map separateCells printedRows

      delimitRows r = '|' : (r ++ "|\n")
      pass2         = map delimitRows pass1

      borderLen     = length $ head pass2
      border        = replicate (borderLen - 1) '-'

      pass3         = concat pass2
