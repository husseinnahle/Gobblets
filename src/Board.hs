module Board (Board, newBoard, emptyPositions, canInsertPiece, getPiecesOnBoard, getPiece, insertPiece, movePiece, getAlignments, filterAlignmentsByColor, Position, showPosition, boardHasWinner, getPlayerScore) where


import Deck (Color(..), Size(..), Piece, size, color)
import Data.List (intercalate)


type Cell = [Piece]

showCell :: Cell -> String
showCell [] = "__"
showCell (x:xs) = show x

isEmpty :: Cell -> Bool
isEmpty [] = True
isEmpty _  = False

getPieceInCell :: Cell -> Maybe Piece
getPieceInCell [] = Nothing
getPieceInCell (x:xs) = Just x

getPieceInCellByColor :: Color -> Cell -> Maybe Piece
getPieceInCellByColor _ [] = Nothing
getPieceInCellByColor c (x:xs)
  | (color x) == c = Just x
  | otherwise = getPieceInCellByColor c xs 

canInsertPieceInCell :: Cell -> Piece -> Bool
canInsertPieceInCell cell newPiece
  | cell == [] = True
  | (size pieceInCell) < (size newPiece) = True
  | otherwise = False
    where
      Just pieceInCell = getPieceInCell cell

insertPieceInCell :: Cell -> Piece -> Cell
insertPieceInCell cell piece = piece:cell 

removePieceFromCell :: Cell -> Cell
removePieceFromCell [] = []
removePieceFromCell (x:xs) = xs


type Position = (Int, Int)

showPosition :: Position -> String
showPosition (x, y) = "(" ++ show x ++ ", " ++ show y ++ ")"


data Board = Board [(Cell, Position)]

instance Show Board where
  show (Board b) = intercalate "\n" (getRows cells)
    where
      cells = map fst b

      getRows :: [Cell] -> [String]
      getRows [] = []
      getRows cells = showRow (take 4 cells) : getRows (drop 4 cells)

      showRow :: [Cell] -> String
      showRow row = concat (replicate 20 " ") ++ intercalate " " (map showCell row)

newBoard :: Board
newBoard = Board ([([], (x, y)) | x <- [0..3], y <- [0..3]])

getCell :: Board -> Position -> Cell
getCell (Board ((cell, current): xs)) position
  | position == current = cell
  | otherwise           = getCell (Board xs) position

setCell :: Board -> Position -> Cell -> Board
setCell (Board ((cell, current):xs)) position newCell
  | position == current = Board ((newCell, current):xs)
  | otherwise = Board ((cell, current):rest)
  where
    Board rest = setCell (Board xs) position newCell

getPiece :: Board -> Position -> Maybe Piece
getPiece board position = getPieceInCell (getCell board position)

insertPiece :: Board -> Piece -> Position -> Board
insertPiece (Board board) piece position = Board (map updateCell board)
    where
      updateCell :: (Cell, Position) -> (Cell, Position)
      updateCell (current_cell, current_position)
        | current_position == position = (insertPieceInCell current_cell piece, current_position)
        | otherwise = (current_cell, current_position)

canInsertPiece :: Board -> Position -> Piece -> Bool
canInsertPiece board position piece = canInsertPieceInCell cell piece
  where
    cell = getCell board position

movePiece :: Board -> Position -> Position -> Board
movePiece b p1 p2 = boardAfterMove
  where
    cell = getCell b p1 
    Just pieceToMove = getPieceInCell cell
    newCell = removePieceFromCell cell
    boardAfterRemove = setCell b p1 newCell
    boardAfterMove = insertPiece boardAfterRemove pieceToMove p2

emptyPositions :: Board -> [Position]
emptyPositions (Board board) = map snd $ filter (isEmpty . fst) board

getPiecesOnBoard :: Board -> [Position]
getPiecesOnBoard (Board board) = map snd $ filter (not . isEmpty . fst) board

getLines :: Board -> [[(Cell, Position)]]
getLines b = rows ++ columns ++ diagonals
  where
    rows = [[(getCell b (x, y), (x, y)) | y <- [0..3]] | x <- [0..3]]
    columns = [[(getCell b (x, y), (x, y)) | x <- [0..3]] | y <- [0..3]] 
    diagonals = [[(getCell b (i, i), (i, i)) | i <- [0..3]], [(getCell b (i, 3-i), (i, 3-i)) | i <- [0..3]]]

getAlignments :: Board -> [[(Cell, Position)]]
getAlignments board = map check (getLines board)
  where
    getColor :: (Cell, Position) -> Maybe Color
    getColor (cell, _) = case getPieceInCell cell of
      Nothing -> Nothing
      Just piece -> Just (color piece)

    check :: [(Cell, Position)] -> [(Cell, Position)]
    check [x, y, z, w]
      | cx == cy && cy == cz && cz == cw = [x, y, z, w]
      | cx == cy && cy == cz = [x, y, z]
      | cy == cz && cz == cw = [y, z, w]
      | cx == cy = [x, y]
      | cy == cz = [y, z]
      | cz == cw = [z, w]
      | otherwise = []
      where
        cx = getColor x
        cy = getColor y
        cz = getColor z
        cw = getColor w

filterAlignmentsByColor :: [[(Cell, Position)]] -> Color -> [[(Cell, Position)]]
filterAlignmentsByColor [] _ = []
filterAlignmentsByColor alignments c = map (filter (sameColor c . fst)) alignments
  where
    sameColor :: Color -> Cell -> Bool
    sameColor c cell
      | Just piece <- getPieceInCell cell, c == (color piece) = True
      | otherwise = False

-- Ancienne implémentation pour chercher le score d'une grille mais qui ne prend pas en conidération les pièces recouvertes
-- getScore :: Board -> Color -> Int
-- getScore b c = alignments2 + 10 * alignments3
--   where
--     alignments = filterAlignmentsByColor (getAlignments b) c
--     alignments2 = length $ filter ((==) 2 . length) alignments
--     alignments3 = length $ filter ((==) 3 . length) alignments

boardHasWinner :: Board -> Maybe Color
boardHasWinner board
  | (length $ filter ((==) 4 . length) blackAlignments) > 0 = Just Black
  | (length $ filter ((==) 4 . length) whiteAlignments) > 0 = Just White
  | otherwise = Nothing 
  where
    alignments = getAlignments board
    blackAlignments = filterAlignmentsByColor alignments Black
    whiteAlignments = filterAlignmentsByColor alignments White

getLineScore :: Color -> [Cell] -> Int
getLineScore c cells = countScore (map (getPieceInCellByColor c) cells) 0
  where
    countScore :: [Maybe Piece] -> Int -> Int
    countScore [] counter
      | counter >= 2 = counter
      | otherwise = 0
    countScore (x:xs) counter
      | Just mx <- x, (color mx) == c = countScore xs (counter + 1)
      | counter >= 2 = counter
      | otherwise = countScore xs 0

getPlayerScore :: Board -> Color -> Int
getPlayerScore b c = countScore (map (getLineScore c) cells)
  where
    getCells :: [[(Cell, Position)]] -> [[Cell]]
    getCells = map $ map fst

    occ :: Eq a => a -> [a] -> Int
    occ _ [] = 0
    occ x (y:ys)
      | x == y    = 1 + occ x ys
      | otherwise = occ x ys

    cells = getCells $ getLines b

    countScore l = (occ 2 l) + 10 * (occ 3 l)