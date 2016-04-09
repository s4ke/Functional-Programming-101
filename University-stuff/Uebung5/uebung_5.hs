-- Martin Braun, 1249080
-- 5. Uebung
-- Tic Tac Toe
module Main where

import Data.Either
import Data.Char
import Data.List
import Data.List.Split
import Control.Monad.State
import Prelude hiding (Either)

import System.Random

import Debug.Trace (trace, traceId, traceM)

--------------------------------------------------------------------
--------------------------------------------------------------------
----------------     Data structures & Logic     -------------------
--------------------------------------------------------------------
--------------------------------------------------------------------

data GameState = Game { board :: Board, currentPlayer :: Player, errorMessage :: String }
type Board = [[Value]]
type Value = Either Int Player
data Player = X | O deriving (Eq, Show)

instance Show GameState where
    show game = x ++ "current Player: " ++ (show $ currentPlayer game) ++ "\nerrorMessage: " ++ (errorMessage game) ++ "\n"
        where x = unlines $ filter (\x -> x /= "") $ foldl (\rest cur -> rest ++ [showLine cur]) [""] (board game)
                where
                    showLine :: [Value] -> String
                    showLine [x, y, z] = (pr x) ++ (pr y) ++ (pr z)
                        where 
                            pr (Right v) = show v
                            pr (Left v) = show v

-- returns the other player for the current player
other :: Player -> Player
other X = O
other O = X

players :: [Player]
players = [X, O]

-- the starting point of our game
initialBoard :: Board
initialBoard = [map Left [1, 2, 3],
                map Left [4, 5, 6],
                map Left [7, 8, 9]]
                
-- sets a value to the specified slot of a game
setTo :: Int -> Player -> [[Value]] -> [[Value]]
setTo at val xs = splitEvery 3 (go at val (concat xs))
    where
        go 1 val (x:xs) = (Right val):xs
        go _ _ [] = [] -- hack
        go at val (x:xs) = x:(go (at - 1) val xs)

-- calculates all possible solutions (ordered by winning < nothing < losing)
possibleSolutions :: Player -> Board -> [Board]
possibleSolutions player board =
    [setTo pos player board | (pos, player, board) <- (sortBy cmp [(pos, player, board) | pos <- [k | k <- (empty board)]])]
        where cmp (pos1, player1, board1) (pos2, player2, board2) 
                = compare (value pos1 player1 board1) (value pos2 player2 board2)
                
deterministicKI :: Player -> Board -> Board
deterministicKI player board = head $ possibleSolutions player board

-- if we win the next turn, we prefer this choice
-- the next most important thing is to prevent us from losing
-- otherwise we are indifferent what we do
value :: Int -> Player -> Board -> Int
value pos player board | win player (setTo pos player board) = -9999
            | lose player (setTo pos (other player) board) = -8888
            | otherwise = 0
-- utility functions to convert the matrix into columns, rows and diagonals
-- this is useful to determine whether there is a winner
col = transpose
row = id
diag [[xx,  _, xz],
      [ _, yy,  _],
      [zx,  _, zz]] = [[xx, yy, zz], [zx, yy, xz]]
      
-- check whether a player has lost the game
lose :: Player -> Board -> Bool
lose player board = win (other player) board

-- check whether a player has won the game
win :: Player -> Board -> Bool
win player board = foldr 
        (\x y -> (winList player x) || y) 
        False 
        ((diag board)++(row board)++(col board))
    where
        winList player xs = 
            foldr (\x y -> (x == (Right player)) && y) True xs

-- check whether the game resulted in a draw
-- this can only be the case if the board has no empty slots left
draw :: Board -> Bool
draw board = 
    ((empty board) == []) &&
    foldr (\player soFar -> (not (win player board)) && soFar) True players
    
over :: Board -> Bool
over board = (empty board) == [] || (draw board) || (win X board) || (win O board)
            
-- utility function that returns all open slot indices            
empty :: Board -> [Int]
empty board = [k | Left k <- concat board]

-- first checks if the slot is empty and then sets it
-- if it's not empty returns Nothing
checkAndSetTo :: Int -> Player -> Board -> (Player, Board, String)
checkAndSetTo pos player board 
    | [pos] `intersect` (empty board) == [pos] = ((other player), (setTo pos player board), "")
    | otherwise = (player, board, "Position already occupied!")
        
--------------------------------------------------------------------
--------------------------------------------------------------------
-----------------           Game cycle          --------------------
--------------------------------------------------------------------
--------------------------------------------------------------------

playGame :: String -> State GameState GameState
playGame [] = do
    game <- get
    put game
    return game
playGame (x:xs) = do
        let pos = digitToInt x
        game <- get
        let curboard = board game
        let player = currentPlayer game
        
        let (nextPlayer, newBoard, error) = checkAndSetTo pos player curboard
        
        let resBoard = newBoard
        let resGame = Game {board = resBoard, 
                            currentPlayer = nextPlayer,
                            errorMessage = error}
        put resGame
        playGame xs
    
initGameState :: GameState
initGameState = Game {board = initialBoard, currentPlayer = X, errorMessage = ""}

limitLength :: String -> Int -> String
limitLength str max = reverse $ go str max
        where   go [] max = []
                go _ 0 = []
                go (x:xs) max = [x] ++ (go xs (max - 1)) 

-- Lösung ohne Interact, ich weiß
playGameIO :: GameState -> IO()
playGameIO game = do
    let curPlayer = currentPlayer game
    putStrLn $ "Player " ++ (show curPlayer) ++ ", please input your turn"
    input <- getLine
    let inputLimited = limitLength input 1
    let (nextState, _) = runState ( do playGame inputLimited ) game
    let resBoard = board nextState
    putStrLn $ show nextState
    if (win curPlayer resBoard) then putStrLn ("Player " ++ (show curPlayer) ++ " wins!") else putStrLn ""
    if (over resBoard) then return () else playGameIO nextState
    
main = do
    putStrLn (show initGameState)
    playGameIO initGameState