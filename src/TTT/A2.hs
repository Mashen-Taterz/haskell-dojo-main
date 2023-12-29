module TTT.A2 where

import Data.List (intercalate)
import TTT.A1
import Data.Char (digitToInt)
import Control.Applicative (Alternative(empty))
import Data.Foldable (Foldable(toList))

-- Q#01
promptPlayer :: Player -> String 
promptPlayer player = 
    concat ["Player ", show player, "'s turn: enter a row and column position."]

-- Q#02
_RANGE_ :: [Int]
_RANGE_ = [0 .. 2]

-- Q#03
isDigit :: Char -> Bool
isDigit digit = digit `elem` ['0' .. '9']

readDigit :: Char -> Int
readDigit digit
    | isDigit digit = read [digit] :: Int 
    | otherwise = -1

-- Q#04
_EMPTY_ROW_ :: [Square]
_EMPTY_ROW_ = replicate _SIZE_ Empty 

_EMPTY_BOARD_:: [[Square]]
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_ 

-- Q#05
isTied :: Board -> Bool
isTied board = Empty `notElem` concat board    

_TIED_BOARD_ :: Board 
_TIED_BOARD_ = [
    [X, O, O]
  , [O, X, X]
  , [O, X, O]
  ]

-- Q#06
indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings strings = zip ['A' ..] strings 
 

-- Q#07
formatLine :: [String] -> String
formatLine line = _SEP_ ++ intercalate _SEP_ line ++ _SEP_

-- Q#08
isMoveInBounds :: Move -> Bool
isMoveInBounds (row , col) =
    inBounds row && inBounds col
    where 
        inBounds index = index >= 0 && index < _SIZE_

-- Q#09

stringToMove = undefined

-- Q#10

replaceSquareInRow = undefined