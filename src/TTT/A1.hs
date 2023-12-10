module TTT.A1 where

import Data.Char (toUpper)


-- Q#01
_SIZE_ :: Int
_SIZE_ = 3

-- Q#02
_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True

-- Q#03
convertRowIndex :: Char -> Int
convertRowIndex c = fromEnum (toUpper c) - 65

-- Q#04
_INVALID_MOVE_ :: (Int , Int)
_INVALID_MOVE_ = (-1 , -1)

-- Q#05
_SEP_ :: [Char]
_SEP_ = "_|_"

-- Q#06

data Square = X | O | Empty deriving (Show , Eq)

-- Q#07

data GameState = XWon | OWon | TieGame | InProgress deriving (Show , Eq)

-- Q#08
type Player = Square 
type Row = [Square]
type Line = [Square]
type Board = [Row]
type Move = (Int, Int)

-- Q#09

getFirstPlayer :: Bool -> Player 
getFirstPlayer isFirstPlayer = 
    if isFirstPlayer 
        then X 
        else O

getFirstPlayer_ :: Bool -> Player
getFirstPlayer_ firstPlayer 
    | firstPlayer = X
    | otherwise = O


-- Q#10

showGameState = undefined

-- Q#11

switchPlayer = undefined

-- Q#12

showSquare = undefined