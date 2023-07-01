module Player (Player(Player), color, newPlayer, showDeck, getDeck, dropPiece) where


import Deck (Color, Piece, Deck, newDeck, getPieces, removePiece)


data Player = Player {color :: Color, deck :: Deck}

instance Show Player where
  show player = show (color player)

newPlayer :: Color -> Player
newPlayer c = Player c (newDeck c)

showDeck :: Player -> String
showDeck player = show (deck player)

getDeck :: Player -> [Piece]
getDeck player = getPieces (deck player)

dropPiece :: Player -> Piece -> Player
dropPiece player piece = Player (color player) (removePiece (deck player) piece)