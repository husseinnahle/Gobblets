module Deck (Color(..), Size(..), Piece(Piece), color, size, Deck, newDeck, getPieces, removePiece) where


import Data.List (intercalate)


data Color = Black | White deriving (Eq)

instance Show Color where
  show Black = "X"
  show White = "O"


data Size = Tiny | Small | Medium | Big deriving (Eq, Ord)

instance Show Size where
  show Tiny = "0"
  show Small = "1"
  show Medium = "2"
  show Big = "3"


data Piece = Piece {color::Color, size::Size} deriving (Eq)

instance Show Piece where
  show (Piece c s) = (show c) ++ (show s)

instance Ord Piece where
  compare p1 p2
    | (size p1) > (size p2) = GT
    | (size p1) < (size p2) = LT
    | otherwise = EQ


data Deck = Deck [[Piece]]

instance Show Deck where
  show deck = intercalate " " [show piece | piece <- getPieces deck]

newDeck :: Color -> Deck
newDeck c = Deck (replicate 3 [(Piece c s) | s <- [Big, Medium, Small, Tiny]])

getPieces :: Deck -> [Piece]
getPieces (Deck pieces) = concat (map (take 1) pieces)

removePiece :: Deck -> Piece -> Deck
removePiece (Deck pieces) p = Deck (remove pieces p)
  where
    remove :: [[Piece]] -> Piece -> [[Piece]] 
    remove [] _ = []
    remove (x:xs) pieceToRm
      | pieceToRm `elem` take 1 x = drop 1 x: xs
      | otherwise         = x: remove xs pieceToRm
