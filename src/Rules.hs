-- | Modules qui fournit les fonctions pour gérer les règles du jeu
module Rules (availableMoves, showAvailableMoves, applyMoves, applyMove) where

import qualified Data.Set as S
import Data.Maybe (fromJust)
import Control.Monad (foldM)

import Move (Move, Drop, OnBoard, showMove)
import Deck (Piece, color, size)
import State (State, newState, board, current, other, playDrop, playOnBoard, hasWinner)
import Player (color, getDeck)
import Board (Position, emptyPositions, getPiecesOnBoard, getPiece, canInsertPiece, getAlignments, filterAlignmentsByColor)


-- | Applique un mouvement à un état de jeu et retourner le nouveau état
applyMove :: State -> Move -> Maybe State
applyMove state move
  | not $ moveIsValid state move = Nothing
  | otherwise = case move of
      Left dropMove -> Just (playDrop state dropMove)
      Right onboardMove -> Just (playOnBoard state onboardMove)

-- | Appliquer une liste de mouvement et retourner l'état final du jeu
applyMoves :: [Move] -> Maybe State
applyMoves = foldM applyMove newState

-- | Vérifier si un mouvement est valide dans un état de jeu
moveIsValid :: State -> Move -> Bool
moveIsValid state move = move `elem` availableMoves state

-- | Récupérer les mouvements possibles dans un état de jeu
availableMoves :: State -> S.Set Move
availableMoves state =
  case hasWinner state of
    Just _ -> S.fromList []
    Nothing -> S.fromList $ [Left x | x <- drops] ++ [Right y | y <- onBoards]
      where
        drops = availableDrops state
        onBoards = availableOnBoards state

-- | Récupérer les mouvements du type Drop possibles dans un état de jeu
availableDrops :: State -> [Drop]
availableDrops state = [(size piece, pos) | piece <- pieces, pos <- positions, canDrop pos piece]
  where
    colorOther = Player.color (other state)

    alignments = filterAlignmentsByColor (getAlignments $ board state) colorOther
    formatedAlignments = concatMap (map snd) (filter ((==) 3 . length) alignments)

    availablePositions = emptyPositions (board state)

    positions = formatedAlignments ++ availablePositions
    pieces = getDeck (current state)

    canDrop :: Position -> Piece -> Bool
    canDrop = canInsertPiece (board state)

-- | Récupérer les mouvements du type OnBoard possibles dans un état de jeu
availableOnBoards :: State -> [OnBoard]
availableOnBoards state = [(p1, p2) | p1 <- src, p2 <- dst, canDrop p1 p2 && p1 /= p2]
  where
    colorCurrent = Player.color (current state)
    pieces = getPiecesOnBoard (board state)
    src = filter (\p -> colorCurrent == Deck.color (fromJust (getPiece (board state) p))) pieces
    dst = [(x, y) | x <- [0..3], y <- [0..3]]

    canDrop :: Position -> Position -> Bool
    canDrop p1 p2 = canInsertPiece (board state) p2 (fromJust (getPiece (board state) p1))

-- | Afficher les mouvement possible
showAvailableMoves :: State -> String
showAvailableMoves state = moveToStr $ S.toList (availableMoves state)
  where
    moveToStr [] = ""
    moveToStr (x:xs) = showMove x ++ "\n" ++ moveToStr xs
