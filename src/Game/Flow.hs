module Game.Flow where

import Game.Board
import Game.RevealTiles
import Game.SetupBoard
import Game.Status
import Game.Tile
import Control.Monad.State.Lazy
import System.Exit
import System.Console.Haskeline
import Data.Text (unpack, split, pack)

type GameFlow = StateT Board IO ()

minesweeper :: Int -> Int -> IO ()
minesweeper rows cols = evalStateT gameFlow $ mb10--emptyBoard rows cols
  where
    mb = emptyBoard rows cols
    mb2 = updateAt mb  (1,1) Mine
    mb3 = updateAt mb2 (0, 3) Mine
    mb4 = updateAt mb3 (5, 5) Mine
    mb5 = updateAt mb4 (7, 7) Mine
    mb6 = updateAt mb5 (0, 0) Mine
    mb7 = updateAt mb6 (0, 6) Mine
    mb8 = updateAt mb7 (2, 6) Mine
    mb9 = updateAt mb8 (7, 5) Mine
    mb10 = setupBoard mb9

printStatus :: BoardIdx -> Either Defeat Board -> GameFlow
printStatus move (Left _) = do
  board <- get
  lift . print $ revealAllMines board
  lift $ putStrLn "Defeat!"
  lift exitSuccess
printStatus _ (Right b)   =
  if clear b
    then do
      lift $ putStrLn  "Victory!"
      lift $ print b
      lift exitSuccess
    else put b

getInput :: IO (Int, Int)
getInput = do
  line <- runInputT defaultSettings $ getInputLine "Your move: "
  return $ processLn line
  where
    processLn Nothing   = undefined
    processLn (Just ln) = toTup
                        . map (read . unpack)
                        . split (== ',')
                        $ pack ln
    toTup [x, y] = (x, y)

gameFlow :: GameFlow
gameFlow = do
  board <- get
  lift $ print board
  move <- lift getInput
  printStatus move $ revealTiles move board
  gameFlow
