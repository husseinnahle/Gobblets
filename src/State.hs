module State (State(State), board, current, other, newState, playDrop, playOnBoard, hasWinner, score) where

import Move (Drop, OnBoard)
import Board (Board, newBoard, insertPiece, movePiece, getPlayerScore, boardHasWinner)
import Player (Player, color, newPlayer, showDeck, dropPiece)
import Deck (Color(Black, White), Piece(Piece))

data State = State {board :: Board, current :: Player, other :: Player}

instance Show State where
  show state = 
    let
      turn = "Turn: Player " ++ show (current state)
      scoreState = concat (replicate 21 " ") ++ "Score: " ++ show (score state)
      boardStr = "\n\n" ++ show (board state) ++ "\n\n"
      decks = (showDeck (current state)) ++ " || " ++ (showDeck (other state))
    in (foldl1 (++) [turn, scoreState, boardStr, decks])

newState :: State
newState = State newBoard (newPlayer Black) (newPlayer White)

playDrop :: State -> Drop -> State
playDrop state (size, position) = State updatedBoard (other state) updatedPlayer
  where
    colorCurrent = color $ current state
    piece = Piece colorCurrent size
    updatedBoard = insertPiece (board state) piece position
    updatedPlayer = dropPiece (current state) piece

playOnBoard :: State -> OnBoard -> State
playOnBoard (State b c o) (p1, p2) = State updateBoard o c
  where
    updateBoard = movePiece b p1 p2

hasWinner :: State -> Maybe Player
hasWinner state =
  case boardHasWinner (board state) of
    Nothing -> Nothing
    Just winnerColor ->
      if winnerColor == color (current state) then Just (current state)
      else Just (other state)

score :: State -> Int
score state = currentScore - otherScore
  where
    currentScore = getPlayerScore (board state) (Player.color $ current state)
    otherScore = getPlayerScore (board state) (Player.color $ other state)
