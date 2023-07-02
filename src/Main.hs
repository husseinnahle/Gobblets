-- | Module principal pour lancer le jeu
module Main (main) where

import State (State, newState, hasWinner)
import Move (Move, getMove)
import Rules (applyMove, availableMoves)

-- | Jouer un mouvement 
playMove :: IO Move
playMove = do
  move <- getMove
  case move of
    Just m -> return m
    Nothing -> error "Invalid move"

-- | Démarrer une partie
run :: State -> IO ()
run state =
  case hasWinner state of
    Just player -> putStrLn $ show state ++ "\nWinner: " ++ show player
    Nothing -> do
      print state
      putStrLn $ show (length $ availableMoves state) ++ " available moves"
      move <- playMove
      case applyMove state move of
        Just new -> run new
        Nothing -> error "Invalid move"

-- | Démarrer une nouvelle partie jeu
main :: IO ()
main = run newState
