module Main where

import Lib
import Data
import System.IO

main :: IO ()
main = do
  let game = makeGame grid languages
  hSetBuffering stdout NoBuffering
  playTurn game

playTurn game = do
  (putStrLn . formatGame) game
  putStrLn "Please enter a word >"
  word <- getLine
  let newGame = playGame game word
  putStrLn . formatGame $ newGame
  if completed newGame
    then putStrLn "Congratulations"
    else playTurn newGame
