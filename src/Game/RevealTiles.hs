module Game.RevealTiles
(
  revealTiles
)
where

import Game.Board
import Game.Tile
import Game.Status

revealTiles :: BoardIdx -> Board -> Either Defeat Board
revealTiles i board = revealFrom tile
  where tile = tileAt board i
        revealFrom Mine         = Left Defeat
        revealFrom (Revealed _) = Right board
        revealFrom (Hidden   _) =
          Right . revealRecursive i . updateAt board i $ reveal tile

revealRecursive :: BoardIdx -> Board -> Board
revealRecursive (row, col) board =
  revealInBounds (row + 1, col)     bs $
  revealInBounds (row + 1, col + 1) bs $
  revealInBounds (row,     col + 1) bs $
  revealInBounds (row - 1, col + 1) bs $
  revealInBounds (row - 1, col)     bs $
  revealInBounds (row - 1, col - 1) bs $
  revealInBounds (row,     col - 1) bs $
  revealInBounds (row + 1, col - 1) bs board
  where bs = getBounds board

revealInBounds :: BoardIdx -> Bounds -> Board -> Board
revealInBounds i@(row, col) ((minRow, minCol) , (maxRow, maxCol)) board
  | row < minRow = board
  | row > maxRow = board
  | col < minCol = board
  | col > maxCol = board
  | otherwise    =
    case tile of
      Mine         -> board
      (Revealed _) -> board
      (Hidden   0) -> revealRecursive i newBoard
      (Hidden   _) -> newBoard
  where
    tile = tileAt board i
    newBoard = updateAt board i $ reveal tile
