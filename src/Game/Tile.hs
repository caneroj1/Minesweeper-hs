{-# LANGUAGE CPP #-}

module Game.Tile
(
  Tile(..)
, reveal
)
where

import Data.String.Unicode (utf8ToUnicodeEmbedErrors)
import Data.Either (rights)

data Tile = RevealedMine |
            Mine         |
            Revealed Int |
            Hidden Int   |
            FlaggedMine  |
            FlaggedTile Int
            deriving Eq

instance Show Tile where
  show (FlaggedTile  _) = flag
  show FlaggedMine      = flag
  show RevealedMine     = bomb
  show Mine             = " "
  show (Revealed n)     = show n
#ifdef    DEBUG
  show (Hidden   n)     = "H" ++ show n
#else
  show (Hidden   _)     = " "
#endif

bomb = rights $ utf8ToUnicodeEmbedErrors ['\xF0', '\x9F', '\x92', '\xA3']
flag = rights $ utf8ToUnicodeEmbedErrors ['\xE2', '\x9A', '\x91']

reveal :: Tile -> Tile
reveal Mine           = RevealedMine
reveal (Hidden n)     = Revealed n
reveal tile           = tile
