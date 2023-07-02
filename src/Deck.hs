-- | Module qui fournit le fonction pour gérer les pieces d'un joueur
module Deck (Color(..), Size(..), Piece(Piece), color, size, Deck, newDeck, getPieces, removePiece) where


-- | Énumérations de couleurs possibles
data Color = Black | White deriving (Eq)

instance Show Color where
  show Black = "X"
  show White = "O"

-- | Énumérations de tailles possibles par ordre croissant de grandeur 
data Size = Tiny | Small | Medium | Big deriving (Eq, Ord)

instance Show Size where
  show Tiny = "0"
  show Small = "1"
  show Medium = "2"
  show Big = "3"

-- | Représentation d'une piece
data Piece = Piece {
  -- | Couleur d'une piece
  color::Color,
  -- | Taille d'une piece
  size::Size
} deriving (Eq)

instance Show Piece where
  show (Piece c s) = show c ++ show s

instance Ord Piece where
  compare p1 p2
    | size p1 > size p2 = GT
    | size p1 < size p2 = LT
    | otherwise = EQ

-- | Représentation des pieces du jeu
newtype Deck = Deck [[Piece]]

instance Show Deck where
  show deck = unwords [show piece | piece <- getPieces deck]

-- | Créer les pieces du jeu d'une couleur
newDeck :: Color -> Deck
newDeck c = Deck (replicate 3 [Piece c s | s <- [Big, Medium, Small, Tiny]])

-- | Récupérer les pieces d'un jeu. Seulement les pieces non recouvertent sont retournées  
getPieces :: Deck -> [Piece]
getPieces (Deck pieces) = concatMap (take 1) pieces

-- | Retirer une pièce d'un jeu
removePiece :: Deck -> Piece -> Deck
removePiece (Deck pieces) p = Deck (remove pieces p)
  where
    remove :: [[Piece]] -> Piece -> [[Piece]] 
    remove [] _ = []
    remove (x:xs) pieceToRm
      | pieceToRm `elem` take 1 x = drop 1 x: xs
      | otherwise         = x: remove xs pieceToRm
