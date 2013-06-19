module Reversi where

import Control.Monad
import Data.List

-- Position type and utility functions
type Position = (Int, Int)

-- ***
-- Given a Position value, determine whether or not it is a legal position on the board
isValidPos :: Position -> Bool
isValidPos a = True

-- Player type and utility functions
data Player = PlayerWhite | PlayerBlack | PlayerNone deriving (Eq)
instance Show Player where
        show PlayerWhite = "white"
        show PlayerBlack = "black"
        show PlayerNone  = "none"


-- ***
-- Given a Player value, return the opponent player
otherPlayer :: Player -> Player
otherPlayer player
    | player == PlayerBlack = PlayerWhite
    | otherwise = PlayerBlack

-- Piece type and utility functions
data Piece = Piece Position Player deriving (Eq)
instance Show Piece where
        show (Piece _ PlayerWhite) = " W"
        show (Piece _ PlayerBlack) = " B"
        show (Piece _ PlayerNone)  = " +"

piecePos :: Piece -> Position
piecePos (Piece pos _) = pos

-- ***
-- Given a Player value and a Piece value, does this piece belong to the player?
isPlayer :: Player -> Piece -> Bool
isPlayer player piece
    | playerOf piece == player = True
    | otherwise = False

-- ***
-- Given a Piece value, determine who the piece belongs to
playerOf :: Piece -> Player
playerOf (Piece _ player) = player

-- ***
-- Flip a piece over
flipPiece :: Piece -> Piece
flipPiece piece
    | playerOf piece == PlayerBlack = Piece (piecePos piece) PlayerWhite
    | playerOf piece == PlayerWhite = Piece (piecePos piece) PlayerBlack
    | otherwise = piece


-- Board type and utility functions
type Board = [Piece]

-- The initial configuration of the game board
initialBoard :: Board
initialBoard =
        [
                Piece (3,4) PlayerWhite, Piece (4,4) PlayerBlack,
                Piece (3,3) PlayerBlack, Piece (4,3) PlayerWhite
        ]

-- ***
-- Given a Position value, is there a piece at that position?
isOccupied :: Position -> Board -> Bool
isOccupied pos boa
 | (length (filter (\x -> piecePos x == pos) boa) >= 1) = True
 | otherwise = False

-- ***
-- Which piece is at a given position? 
-- Return Nothing in the case that there is no piece at the position
-- Otherwise return Just the_piece

pieceAt :: Position -> Board -> Piece
pieceAt pos board
    | (length (filter (\x -> piecePos x == pos) board) == 1) = (filter (\x -> piecePos x == pos) board) !! 0
    | otherwise = Piece (100, 100) PlayerNone

-- ***
-- Determine if a particular piece can be placed on a board.  
-- There are two conditions: 
-- (1) no two pieces can occupy the same space, and 
-- (2) at least one of the other player's pieces must be flipped by the placement of the new piece.
validMove :: Piece -> Board -> Player -> Bool
validMove pie boa pla
    | (length (filter (\x -> piecePos x == piecePos pie) boa) >= 1) = False
    | pie `elem` (allMoves pla boa) = True
    | otherwise = False

-- ***
-- Determine which pieces would be flipped by the placement of a new piece
toFlip :: Piece -> Board -> [Piece]
toFlip piece board
    = [ y | k<-[-1..1], l<-[-1..1], y <- flippable piece (getLineDir (k,l) piece board)]

-- ***
-- Auxillary function for toFlip. 
-- You don't have to use this function if you prefer to define toFlip some other way.
-- Determine which pieces might get flipped along a particular line 
-- when a new piece is placed on the board.  
-- The first argument is a vector (pair of integers) that describes 
-- the direction of the line to check.  
-- The second argument is the hypothetical new piece.  
-- The return value is either the empty list, 
-- a list where all pieces belong to the same player, 
-- or a list where the last piece belongs to the player of the hypothetical piece.  
-- Only in the last case can any of the pieces be flipped.
-- (Int, Int) can only be [(x, y) | x<-[-1..1], y<-[-1..1]]

getLineDir :: (Int, Int) -> Piece -> Board -> [Piece]
getLineDir direct piece board
    | direct == (0, 0) = []
    | otherwise = [ y | let piePos = piecePos piece, z <- [1..7], y <- filter (\x -> piecePos x == (fst(piePos) + z * fst(direct), snd(piePos) + z * snd(direct) )) board]

-- ***
-- Auxillary function for toFlip.
-- You don't have to use this function if you prefer to define toFlip some other way.
-- Given the output from getLineDir, determine which, if any, of the pieces would be flipped.

flippable :: Piece -> [Piece] -> [Piece]
flippable piece pieceList
    | length (filter(\x -> playerOf x == playerOf piece) pieceList) == 0 = []
    | otherwise = filter(\x -> playerOf x == otherPlayer(playerOf piece)) pieceList

-- ***
-- Place a new piece on the board.  Assumes that it constitutes a validMove
makeMove :: Piece -> Board -> Board
makeMove pie boa
    | validMove pie boa (playerOf pie) = boa ++ [pie]
    | otherwise = boa


-- ***
-- Find all valid moves for a particular player
allMoves :: Player -> Board -> [Piece]
allMoves pla boa =
    [ Piece (fst(piePos)+x, snd(piePos)+y) pla | x <- [-1, 0, 1], y <- [-1, 0, 1], piePos <- [ piecePos z | z <- (filter (\x -> playerOf x == (otherPlayer pla)) boa)], not (isOccupied (fst(piePos)+x, snd(piePos)+y) boa), let piece = Piece (fst(piePos)+x, snd(piePos)+y) pla, length(flippable piece (getLineDir (0-x, 0-y) piece boa)) > 0]

-- ***
-- Count the number of pieces belonging to a player
score :: Player -> Board -> Int
score player board = length (filter(\x -> playerOf x == player) board)

-- ***
-- Decide whether or not the game is over. The game is over when neither player can make a validMove
isGameOver :: Board -> Bool
isGameOver board
    | (length (allMoves PlayerBlack board)) >= 0 = False
    | (length (allMoves PlayerWhite board)) >= 0 = False
    | otherwise = True

-- ***
-- Find out who wins the game.  
-- Return Nothing in the case of a draw.
-- Otherwise return Just the_Player
winner :: Board -> Maybe Player
winner board
    | (score PlayerBlack board) > (score PlayerWhite board) = Just PlayerBlack
    | (score PlayerBlack board) < (score PlayerWhite board) = Just PlayerWhite
    | otherwise = Nothing
