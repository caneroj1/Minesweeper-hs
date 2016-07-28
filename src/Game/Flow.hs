module Game.Flow where

import Game.Board
import Game.RevealTiles
import Game.SetupBoard
import Game.Status
import Game.FlagTiles
import Game.Tile
import Game.Move
import Control.Monad.State.Lazy
import System.Exit
import System.Console.Haskeline
import Data.Text (unpack, split, pack)
import System.Random (getStdGen)
import System.Console.ANSI (clearScreen, setCursorPosition)

type GameFlow = StateT Board IO ()

resetConsole = clearScreen >> setCursorPosition 0 0

minesweeper :: Int -> Int -> Int -> IO ()
minesweeper rows cols mines = do
  resetConsole
  gen          <- getStdGen
  let playingBoard = setupBoard mines board gen
    in
      evalStateT gameFlow playingBoard
  where
    board        = emptyBoard rows cols

printStatus :: Either Defeat Board -> GameFlow
printStatus (Left _) = do
  board <- get
  lift . print $ revealAllMines board
  lift $ putStrLn "Defeat!"
  lift exitSuccess
printStatus (Right b)   =
  if clear b
    then do
      lift $ putStrLn  "Victory!"
      lift $ print b
      lift exitSuccess
    else put b

getInput :: IO Move
getInput = do
  line <- runInputT defaultSettings $ getInputLine "Action: "
  return $ toMove line
  where
    toMove Nothing       = None
    toMove (Just string) = read string

execMove :: Move -> Board -> GameFlow
execMove (RevealTile i) board = printStatus $ revealTiles i board
execMove (FlagTile   i) board = put $ flag i board
execMove (UnflagTile i) board = put $ unflag i board
execMove None       _     = return ()

gameFlow :: GameFlow
gameFlow = do
  board <- get
  lift $ print board
  move <- lift getInput
  execMove move board
  lift resetConsole
  gameFlow
