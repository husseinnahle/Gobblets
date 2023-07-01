module Main (main) where

import State (State, newState, playDrop, playOnBoard, hasWinner)
import Move (Move, getMove)
import Rules (applyMove, availableMoves)
import Deck (Color)

playMove :: IO Move
playMove = do
    putStrLn ""
    move <- getMove
    case move of
      Just m -> return m
      Nothing -> do
        putStrLn "Invalid move"
        playMove

run :: State -> IO ()
run state = 
  case hasWinner state of
    Just player -> putStrLn $ show state ++ "\nWinner: " ++ show player
    Nothing -> do
      putStrLn $ show state
      -- putStrLn $ showAvailableMoves state
      putStrLn $ show (length $ availableMoves state) ++ " available moves"
      move <- playMove
      let newState = applyMove state move
      case newState of
        Just new -> run new
        Nothing -> do
          putStrLn "Invalid move"
          run state

main :: IO ()
main = run newState
