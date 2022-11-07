module BlackJack where

import Cards 
import RunGame 
import Test.QuickCheck

-- ghci -package random
-- :m System.Random
-- hand1 = Hand (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))

-- A0 --
{-
size hand2
  = size (Add (Card (Numeric 2) Hearts)
              (Add (Card Jack Spades) Empty))
  = 1 + size (Add (Card Jack Spades) Empty)
  = 1 + 1 + size (Empty)
  = 1 + 1 + 0
  = 2
-}

-- A1 --
displayCard :: Card -> String
displayCard (Card (Numeric r) s)   = "" ++ show (r) ++ " of " ++ show (s)
displayCard (Card r s)             = "" ++ show (r) ++ " of " ++ show (s)

display :: Hand -> String
display Empty            = ""
display (Add card hand)  = displayCard (card) ++ ", " ++ display hand

-- A2 --
cardValue :: Card -> Integer -> Integer
cardValue (Card (Numeric r)  _) (_)  = r
cardValue (Card Ace _) (aceValue)  = aceValue
cardValue (Card _ _) (_)           = 10

initialValue :: Hand -> Integer -> Integer
initialValue (Empty) (_)           = 0
initialValue (Add card hand) (ace)  = cardValue (card) (ace) + initialValue (hand) (ace)

value :: Hand -> Integer
value hand =
              case initialValue hand 11 > 21 of
                             True -> initialValue hand 1
                             False  -> initialValue hand 11

-- A3 --
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- A4 --
winner :: Hand -> Hand -> Player
winner guest bank
    | not (gameOver guest) && (gameOver bank)                  = Guest
    | not (gameOver guest) && not (value guest <= value bank)  = Guest
    | otherwise                                                = Bank



