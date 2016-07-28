{-# LANGUAGE RecordWildCards #-}

module Game.SetupBoard
(
  setupBoard
)
where

import Game.Board
import Game.Tile
import Data.Array
import qualified Data.Sequence as S
import Data.Sequence ((><))
import System.Random

type CountArray = Array BoardIdx Int
type BoardSeq   = S.Seq BoardIdx

setupBoard :: Int -> Board -> StdGen -> Board
setupBoard i b g = finalBoard
  where initialSeq = S.fromList . indices $ tiles b
        minedBoard = addMinesToBoard i b g initialSeq
        finalBoard = setupCounts minedBoard

addMinesToBoard :: Int -> Board -> StdGen -> BoardSeq -> Board
addMinesToBoard 0        board _   _       = board
addMinesToBoard numMines board gen currSeq =
  let (idx, nextGen) = randomR (0, length currSeq - 1) gen
      boardIdx       = S.index currSeq idx
      nextSeq        = deleteAtIdx idx currSeq
      nextBoard      = updateAt board boardIdx Mine
    in
      addMinesToBoard (numMines - 1) nextBoard nextGen nextSeq

splitAtExclusive :: Int -> S.Seq a -> (S.Seq a, S.Seq a)
splitAtExclusive i s = (S.take (i-1) s, S.drop i s)

deleteAtIdx :: Int -> BoardSeq -> BoardSeq
deleteAtIdx i = uncurry (><) . splitAtExclusive i

setupCounts :: Board -> Board
setupCounts board@Board{..} =
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
