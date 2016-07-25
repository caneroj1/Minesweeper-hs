{-# LANGUAGE RecordWildCards #-}

module Game.SetupBoard
(
  setupBoard
)
where

import Game.Board
import Game.Tile
import Data.Array

type CountArray = Array BoardIdx Int

setupBoard :: Board -> Board
setupBoard board@Board{..} =
  board {tiles = newTiles}
  where
    bounds   = getBounds board
    arrItems = getTiles  board
    counts   = assocs $ accumTileCounts bounds arrItems
    newTiles = foldr incIfOpen tiles counts

incIfOpen :: (BoardIdx, Int) -> TwoDArray -> TwoDArray
incIfOpen (i@(row, col), n) arr = modifyWith item
  where item = arr ! (row, col)
        modifyWith Mine         = arr
        modifyWith (Revealed _) = arr // [(i, Revealed n)]
        modifyWith (Hidden   _) = arr // [(i, Hidden n)]

accumTileCounts :: Bounds -> [BoardAssoc] -> CountArray
accumTileCounts bounds assocs =
  foldr updateSurroundingTiles countArray mineIdxs
  where
    countArray = listArray bounds $ repeat 0
    mineIdxs   = map fst $ filter ((== Mine) . snd) assocs

updateSurroundingTiles :: BoardIdx -> CountArray -> CountArray
updateSurroundingTiles (row, col) arr =
  incInBounds (row + 1, col)     bs $
  incInBounds (row + 1, col + 1) bs $
  incInBounds (row,     col + 1) bs $
  incInBounds (row - 1, col + 1) bs $
  incInBounds (row - 1, col)     bs $
  incInBounds (row - 1, col - 1) bs $
  incInBounds (row,     col - 1) bs $
  incInBounds (row + 1, col - 1) bs arr
  where bs = bounds arr

incInBounds :: BoardIdx -> Bounds -> CountArray -> CountArray
incInBounds i@(row, col) ((minRow, minCol) , (maxRow, maxCol)) arr
  | row < minRow = arr
  | row > maxRow = arr
  | col < minCol = arr
  | col > maxCol = arr
  | otherwise    = modifiedArr
  where
    modifiedArr = arr // [(i, prev + 1)]
      where prev = arr ! i
