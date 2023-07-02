-- | Module qui fournit les fonctions pour gérer les mouvements et leurs entrées
module Move (Move, Drop, OnBoard, getMove, showMove, parseMoves) where

import Board (Position, showPosition)
import Deck (Size(Big, Medium, Small, Tiny))

import Data.Char (digitToInt)
import System.IO
import Text.Parsec
import Text.Parsec.String (Parser)

-- | Représentation d'un mouvement Drop
type Drop = (Size, Position)

-- | Afficher un mouvement Drop
showDrop :: Drop -> String
showDrop (size, p) = "drop(" ++ sizeStr size ++ ", " ++ showPosition p ++ ")"
  where
    sizeStr Big = "B"
    sizeStr Medium = "M"
    sizeStr Small = "S"
    sizeStr Tiny = "T"


-- | Représentation d'un mouvement OnBoard
type OnBoard = (Position, Position)

-- | Afficher un mouvement OnBoard
showOnBoard :: OnBoard -> String
showOnBoard (src, dst) = "onBoard(" ++ showPosition src ++ ", " ++ showPosition dst ++ ")"


-- | Représentation d'un mouvement. Soit Drop soit OnBoard
type Move = Either Drop OnBoard

-- | Afficher un mouvement
showMove :: Move -> String
showMove (Left dropMove) = showDrop dropMove
showMove (Right onboardMove) = showOnBoard onboardMove

-- | Parser du séparateur de position
sepParser :: Parser String
sepParser = 
    do skipMany space
       _ <- char ','
       skipMany space
       return ""

-- | Parser de position
positionParser :: Parser Position
positionParser = 
  do _ <- char '('
     skipMany space
     x <- digitToInt <$> oneOf "0123"
     _ <- sepParser
     y <- digitToInt <$> oneOf "0123"
     skipMany space
     _ <- char ')'
     skipMany space
     return (x, y)

-- | Parser de mouvement Drop
dropParser :: Parser Drop
dropParser = 
  do _ <- char '('
     skipMany space
     size <- do
       c <- oneOf "BMST"
       case c of
         'B' -> return Big
         'M' -> return Medium
         'S' -> return Small
         'T' -> return Tiny
         _ -> fail "Invalid size"
     _ <- sepParser
     postition <- positionParser
     _ <- char ')'
     skipMany space
     return (size, postition)

-- | Parser de mouvement OnBoard
onBoardParser :: Parser OnBoard
onBoardParser =
  do _ <- char '('
     skipMany space
     postition1 <- positionParser
     _ <- sepParser
     postition2 <- positionParser
     _ <- char ')'
     skipMany space
     return (postition1, postition2)

-- | Parser d'un mouvement
moveParser :: Parser Move
moveParser = 
  do moveType <- try (string "drop") <|> string "onboard"
     skipMany space
     case moveType of
      "drop" -> Left <$> dropParser
      "onboard" -> Right <$> onBoardParser
      _ -> fail "Invalid move"

-- | Parser d'une liste de mouvement
parseMoves :: [String] -> [Move]
parseMoves = map parseMove
  where
    parseMove :: String -> Move
    parseMove str = case parse moveParser "" str of
      Left _ -> error "Invalid move"
      Right move -> move

-- | Lire un mouvement
getMove :: IO (Maybe Move)
getMove = 
  do putStr "> "
     hFlush stdout
     input <- getLine
     let result = parse moveParser "" input
     case result of
        Left _ -> return Nothing
        Right move -> return (Just move)