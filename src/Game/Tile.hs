{-# LANGUAGE CPP #-}

module Game.Tile where

import Data.String.Unicode (utf8ToUnicodeEmbedErrors)
import Data.Either (rights)

data Tile = RevealedMine | Mine | Revealed Int | Hidden Int deriving Eq

instance Show Tile where
  show RevealedMine = bomb
  show Mine         = " "
  show (Revealed n) = show n
#ifdef    DEBUG
  show (Hidden   n) = "H" ++ show n
#else
  show (Hidden   _) = " "
#endif

bomb = rights $ utf8ToUnicodeEmbedErrors ['\xF0', '\x9F', '\x92', '\xA3']

reveal :: Tile -> Tile
reveal Mine           = Mine
reveal r@(Revealed _) = r
reveal (Hidden n)     = Revealed n
