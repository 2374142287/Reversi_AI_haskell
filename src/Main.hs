module Main where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.IO

import GameTree
import Reversi

computerPlayer :: Player
computerPlayer = PlayerWhite

-- How many moves the computer player will look ahead
lookahead :: Int
lookahead = 3

-- Encodes the initial board state, and a hypothetical initial move.
initialMove :: Game
initialMove = Game (Piece (3, 4) PlayerWhite) initialBoard

-- UI routines
main :: IO ()
main = do
        putStrLn "Welcome to reversi (human vs. computer)"
        putStrLn "Human is playing black, computer is playing white"
        turn initialMove PlayerBlack

-- execute a single turn
turn :: Game -> Player -> IO ()
turn lastMove player =
        -- game over
        if isGameOver board then do
                drawBoard board
                putStrLn "Game over"
                let scoreWhite = score PlayerWhite board
                let scoreBlack = score PlayerBlack board
                let win = maybe ", a tie" (\p -> " to " ++ show p)$ winner board

                putStrLn$ "The score was " ++
                        show scoreWhite ++ " - " ++ show scoreBlack ++ win
                return ()

        -- player cannot make a move, so pass
        else if null (allMoves player board) then do
                putStrLn$ show player ++ " must pass"
                turn lastMove (otherPlayer player)

        -- it is the computer's turn
        else if player == computerPlayer then do
                drawBoard board
                let (Game (Piece pos player') board') = aiMove lookahead player lastMove
                putStrLn$ show computerPlayer ++ " plays " ++ posToCoord pos

                let z = (toFlip (Piece pos player) board)
                let y = (makeMove (Piece pos player) board)
                let x = nub ((y \\ z) ++ [ flipPiece y | y <- z])

                turn (Game (Piece pos player) x) (otherPlayer player)
                --turn (Game (Piece pos player') board') (otherPlayer player)

        -- it is the human player's turn
        else do
                drawBoard board
                move <- getValidMove
                let z = (toFlip move board)
                let y = (makeMove move board)
                let x = nub ((y \\ z) ++ [ flipPiece y | y <- z])
                turn (Game move x) (otherPlayer player)

        -- helper function to validate a move
        where
                board = getBoard lastMove
                getValidMove = do
                        move <- getHumanMove player
                        if validMove move board player
                                then return move
                                        else do
                                                putStrLn "Invalid move, try again"
                                                getValidMove


-- Read a sane (but not necessarily valid) move
getHumanMove :: Player -> IO Piece
getHumanMove player = do
        putStr$ "Enter coords (e.g. a1), " ++ show player ++ " to move: "
        hFlush stdout
        ln <- getLine
        case coordToPos (trim ln) of
                Nothing -> do
                        putStrLn "Invalid coordinates, try again"
                        getHumanMove player
                Just pos -> return$ Piece pos player

-- Helper functions for parsing user input
ltrim :: String -> String
ltrim = dropWhile isSpace

trim :: String -> String
trim = reverse.ltrim.reverse.ltrim

coordToPos :: String -> Maybe Position
coordToPos [col, row] = do
        x <- col `elemIndex` ['a'..'h']
        y <- row `elemIndex` ['1'..'8']
        return (x, y)
coordToPos _ = Nothing

posToCoord :: Position -> String
posToCoord (x, y) = (['a' .. 'h'] !! x) : [['1' .. '8'] !! y]

-- Draw the game board
drawBoard :: Board -> IO ()
drawBoard board = do
        putStrLn "  a b c d e f g h"
        forM (reverse [0..7])$ \y -> drawRow y
        putStrLn "  a b c d e f g h"

        return ()

        where
                drawRow y = do
                        putStr$ show (y + 1)
                        forM [0..7]$ \x -> putStr$ posStr (x, y)
                        putStrLn$ " " ++ (show (y + 1))
                posStr pos = show (pieceAt pos board)


