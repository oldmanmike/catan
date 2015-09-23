{-# LANGUAGE OverloadedStrings #-}
module Main where

import Catan
import Control.Monad
import Data.Char
import Data.List
import Linear (V4(..))
import SDL
import System.IO
import System.Random
import System.Random.Shuffle

main :: IO ()
main = do
  initialize [InitEverything]
  window <- createWindow "Settlers of Catan" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
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
  appLoop renderer


appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = not (null (filter eventIsQPress events))
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  present renderer
  unless qPressed (appLoop renderer)


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


