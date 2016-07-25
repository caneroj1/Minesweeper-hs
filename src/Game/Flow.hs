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
minesweeper rows cols = evalStateT gameFlow $ mb4--emptyBoard rows cols
  where
    mb = emptyBoard rows cols
    mb2 = updateAt mb (1,1) Mine
    mb3 = updateAt mb2 (0, 3) Mine
    mb4 = setupBoard mb3

printStatus :: BoardIdx -> Either Defeat Board -> GameFlow
printStatus move (Left _) = do
  board <- get
  lift . print $ updateAt board move RevealedMine
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
