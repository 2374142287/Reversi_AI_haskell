module GameTree where

import Data.List
import Data.Ord
import Reversi

-- Generic tree datatype
data Tree a = Root a [Tree a] deriving Show

getRoot :: Tree a -> a
getRoot (Root a _) = a

-- Function to build trees recursively.  f takes a tree element, and returns all
-- the child elements.
repTree :: (a -> [a]) -> a -> Tree a
repTree f root = Root root (map (repTree f) (f root))

-- Labels for the game tree.  We need to know the current board, and which was
-- the last piece to be played.
data Game = Game Piece Board deriving Show

getBoard :: Game -> Board
getBoard (Game _ board) = board

-- An adaptor function for allMoves.  It is slightly complicated by the
-- possibility that a player may be forced to skip a turn (if the player cannot
-- make any valid moves)
gameMoves :: Game -> [Game]
gameMoves (Game (Piece _ player) board) =
                map (\piece -> Game piece (makeMove piece board))
                        (if null moves then movesPass else moves)
        where
                moves = allMoves (otherPlayer player) board
                movesPass = allMoves player board

-- Construct a game tree
gameTree :: Game -> Tree Game
gameTree = repTree gameMoves

-- Helper function to find out who's turn it is for any node in the game tree
movePlayer :: Tree Game -> Player
movePlayer (Root (Game (Piece _ player) _) _) = player

-- Estimate the value of a position for a player.  The AI could be improved by
-- taking account of strategic concerns here.  i.e. some pieces have more
-- strategic value than others.
estimate :: Player -> Game -> Int
estimate player (Game _ board) = score player board

-- Maximise or minimise the value of a subtree, depending on whose move it is.
-- The aiPlayer parameter is used to determine which player should be maximised,
-- and which player should be minimised.  It is passed through all the recursive
-- maximise/minimise/minimax calls unchanged.
minimax :: Player -> Tree Game -> Int
minimax aiPlayer node =
        if movePlayer node == aiPlayer then maximise aiPlayer node
        else minimise aiPlayer node

-- Maximise the value of a subtree, return the maximised value.  When we
-- maximise, it is the AI player's turn
maximise :: Player -> Tree Game -> Int
maximise aiPlayer (Root x []) = estimate aiPlayer x
maximise aiPlayer (Root _ subs) = maximum (map (minimax aiPlayer) subs)

-- Minimise the value of a subtree, return the minimised value.  When we
-- minimise, it is the other players turn (not the AI player)
minimise :: Player -> Tree Game -> Int
minimise aiPlayer (Root x []) = estimate (otherPlayer aiPlayer) x
minimise aiPlayer (Root _ subs) = minimum (map (minimax aiPlayer) subs)

-- Use maximise to find the best move.  The best move is the one where maximise
-- returns the highest value
bestMove :: Player -> Tree Game -> Game
bestMove aiPlayer (Root _ subs) =
        getRoot (maximumBy (comparing (maximise aiPlayer)) subs)

-- Prune a game tree to a manageable number of levels
prune :: Int -> Tree Game -> Tree Game
prune 0 (Root x _) = Root x []
prune n (Root x sub)
        | n > 0 = Root x (map (prune (n - 1)) sub)

-- Determine the best move for a player
aiMove :: Int -> Player -> Game -> Game
aiMove lookahead player = bestMove player . prune lookahead . gameTree

