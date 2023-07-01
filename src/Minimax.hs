module Minimax (minimaxScore) where

import State (State, score)

-- | Retourne le score du minimax pour un état 'state', avec une profondeur
-- demandée de recherche 'depth'
minimaxScore :: State -> Int -> Int
minimaxScore state _ = score state
