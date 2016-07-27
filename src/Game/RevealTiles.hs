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
        revealFrom Mine            = Left Defeat
        revealFrom (Revealed _)    = Right board
        revealFrom (Hidden   0)    =
          Right . revealRecursive i . updateAt board i $ reveal tile
        revealFrom (Hidden   _)    = Right . updateAt board i $ reveal tile
        revealFrom FlaggedMine     = Left Defeat
        revealFrom (FlaggedTile 0) =
          Right . revealRecursive i . updateAt board i $ reveal tile
        revealFrom (FlaggedTile _) = Right . updateAt board i $ reveal tile
        -- shouldn't happen. game is over before any revealed mines are
        -- displayed
        revealFrom RevealedMine    = Left Defeat

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
      Mine            -> board
      FlaggedMine     -> board
      (FlaggedTile _) -> board
      (Revealed _)    -> board
      (Hidden   0)    -> revealRecursive i newBoard
      (Hidden   _)    -> newBoard
      -- shouldn't happen. game is over before any revealed mines are
      -- displayed
      RevealedMine    -> board
  where
    tile = tileAt board i
    newBoard = updateAt board i $ reveal tile
