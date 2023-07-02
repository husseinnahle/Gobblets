-- | Module qui fournit la fonction qui récupère le score d'un état de jeu
module Minimax (minimaxScore) where

import State (State, score)

-- | Retourne le score du minimax pour un état 'state'
minimaxScore :: State -> Int -> Int
minimaxScore state _ = score state
