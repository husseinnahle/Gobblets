-- | Module qui fournit les fonctions pour gérer un état de jeu
module State (State, board, current, other, newState, playDrop, playOnBoard, hasWinner, score) where

import Move (Drop, OnBoard)
import Board (Board, newBoard, insertPiece, movePiece, getPlayerScore, boardHasWinner)
import Player (Player, color, newPlayer, showDeck, dropPiece)
import Deck (Color(Black, White), Piece(Piece))

-- | Représentation d'un état de jeu
data State = State {
  -- | Grille de l'état de jeu
  board :: Board,
  -- | Joueur courant
  current :: Player,
  -- | Deuxieme joueur 
  other :: Player
}

instance Show State where
  show state = 
    let turn = "Turn: Player " ++ show (current state)
        scoreState = concat (replicate 21 " ") ++ "Score: " ++ show (score state)
        boardStr = "\n\n" ++ show (board state) ++ "\n\n"
        decks = showDeck (current state) ++ " || " ++ showDeck (other state)
    in foldl1 (++) [turn, scoreState, boardStr, decks]

-- | Créer un jeu
newState :: State
newState = State newBoard (newPlayer Black) (newPlayer White)

-- | Jouer un mouvement de type Drop et retourner le nouveau état
playDrop :: State -> Drop -> State
playDrop state (size, position) = State updatedBoard (other state) updatedPlayer
  where
    colorCurrent = color $ current state
    piece = Piece colorCurrent size
    updatedBoard = insertPiece (board state) piece position
    updatedPlayer = dropPiece (current state) piece

-- | Jouer un mouvement de type OnBoard et retourner le nouveau état
playOnBoard :: State -> OnBoard -> State
playOnBoard (State b c o) (p1, p2) = State updateBoard o c
  where
    updateBoard = movePiece b p1 p2

-- | Vérifier si le jeu possède un gagnat et retourner le
hasWinner :: State -> Maybe Player
hasWinner state =
  case boardHasWinner (board state) of
    Nothing -> Nothing
    Just winnerColor ->
      if winnerColor == color (current state) then Just (current state)
      else Just (other state)

-- | Récupérer le score d'un jeu
score :: State -> Int
score state = currentScore - otherScore
  where
    currentScore = getPlayerScore (board state) (Player.color $ current state)
    otherScore = getPlayerScore (board state) (Player.color $ other state)
