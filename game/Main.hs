module Main where

import Catan
import Data.Char
import Data.List
import Control.Monad
import System.IO
import System.Random
import System.Random.Shuffle


initBoard :: IO Board
initBoard = do
  gen1 <- newStdGen
  gen2 <- newStdGen
  gen3 <- newStdGen
  let map = sort $ zip2Hex clst tlst blst rlst
        where clst = shuffle' coordList 19 gen1
              tlst = (7:) $ shuffle' tokenList 18 gen2
              blst = (Desert:) $ shuffle' biomeList 18 gen3
              rlst = True : (replicate 18 False)
  return $ Board map [] []


main :: IO ()
main = do
  -- Init Board
  board <- initBoard

  -- Init Players
  hSetBuffering stdin NoBuffering
  putStrLn "How many players?"
  numPlayers <- liftM digitToInt getChar
  putStr "\n"
  putStrLn "Please enter player's names:"
  playerNames <- replicateM numPlayers getLine
  let playerColors = if (numPlayers <= 4) && (numPlayers > 0)
                        then Right $ take numPlayers colorList
                        else Left "Error: Can only have 1-4 players!"
  -- Debug
  print board
  --
