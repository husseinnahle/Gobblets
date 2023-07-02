-- | Module qui contient les fonction pour gérer tout ce qui est en rapport avec un joueur
module Player (Player(Player), color, newPlayer, showDeck, getDeck, dropPiece) where

import Deck (Color, Piece, Deck, newDeck, getPieces, removePiece)


-- | Représentation d'un joueur
data Player = Player {
  -- | Couleur du joueur
  color :: Color,
  -- | Main du joueur
  deck :: Deck
}

instance Show Player where
  show player = show (color player)

-- | Créer un nouveau joueur
newPlayer :: Color -> Player
newPlayer c = Player c (newDeck c)

-- | Afficher les pieces disponible d'un joueur
showDeck :: Player -> String
showDeck player = show (deck player)

-- | Récupérer les pièce disponible d'un joueur
getDeck :: Player -> [Piece]
getDeck player = getPieces (deck player)

-- | Retirer une piece d'un joueur
dropPiece :: Player -> Piece -> Player
dropPiece player piece = Player (color player) (removePiece (deck player) piece)
