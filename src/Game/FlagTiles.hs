module Game.FlagTiles
(
  flag
, unflag
)
where

import Game.Board
import Game.Tile

flag :: BoardIdx -> Board -> Board
flag i board = updateAt board i flaggedTile
  where
    flaggedTile = case tileAt board i of
      Mine     -> FlaggedMine
      Hidden n -> FlaggedTile n
      tile     -> tile

unflag :: BoardIdx -> Board -> Board
unflag i board = updateAt board i unflaggedTile
  where
    unflaggedTile = case tileAt board i of
      FlaggedMine   -> Mine
      FlaggedTile n -> Hidden n
      tile          -> tile
