module BlackJack where

import Cards 
import RunGame 
import Test.QuickCheck
import System.Random

-- A0 --
--By Hand
{-
hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)

size hand2
  = size (Add (Card (Numeric 2) Hearts)
              (Add (Card Jack Spades) Empty))
  = 1 + size (Add (Card Jack Spades) Empty)
  = 1 + 1 + size (Empty)
  = 1 + 1 + 0
  = 2
-}

-- As definition
hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)

sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
                        , 1 + size (Add (Card Jack Spades) Empty)
                        , 1 + 1
                        , 2]

-- A1 --
-- | The string representation of a Card
displayCard :: Card -> String
displayCard (Card (Numeric r) s)   = "" ++ show r ++ " of " ++ show s
displayCard (Card r s)             = "" ++ show r ++ " of " ++ show s

-- | The string representation of a hand of Cards
display :: Hand -> String
display Empty            = ""
display (Add card hand)  = displayCard card ++ ", " ++ display hand

-- A2 --
-- | The Integer value of a Card (Second parameter for setting Ace value)
cardValue :: Card -> Integer -> Integer
cardValue (Card (Numeric r)  _) _  = r
cardValue (Card Ace _) aceValue    = aceValue
cardValue (Card _ _) _             = 10

-- | The Integer value of a hand of Cards with the Ace value set in the second parameter
initialValue :: Hand -> Integer -> Integer
initialValue (Empty) _           = 0
initialValue (Add card hand) ace  = cardValue card ace + initialValue hand ace

-- | The Integer value of a hand of Cards
value :: Hand -> Integer
value hand =
              case initialValue hand 11 > 21 of
                             True -> initialValue hand 1
                             False  -> initialValue hand 11

-- A3 --
-- | Boolean value for state of game.
-- GameOver if value of hand exceeds 21, returns true then
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- A4 --
-- | Returns the winner of the game, guest or bank
winner :: Hand -> Hand -> Player
winner guest bank
    | not (gameOver guest) && (gameOver bank)                  = Guest
    | not (gameOver guest) && not (value guest <= value bank)  = Guest
    | otherwise                                                = Bank

-- B1 --
-- | Adds card from first hand on top of the second hand
--   returning the resulting hand
(<+) :: Hand -> Hand -> Hand
(<+) Empty Empty           = Empty
(<+) Empty h2              = h2
(<+) h1 Empty              = h1
(<+) (Add card hand) h2    = Add card (hand <+ h2)

-- tests
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = (size h1 + size h2) == size (h1 <+ h2)

-- B2 --
-- Helper Functions
suit2Hand :: Suit -> Hand
suit2Hand s = suit2Hand' ([Card r s | r <- [Jack, Queen, King, Ace]] ++
                          [Card (Numeric r) s | r <- [2..10]])

suit2Hand' :: [Card]  -> Hand
suit2Hand' cards  = foldr Add Empty cards

-- | Creates and returns a full deck
-- (Calls a helper function on each suit then collects all hands with <+)
fullDeck :: Hand
fullDeck = suit2Hand Hearts <+ suit2Hand Spades
               <+ suit2Hand Diamonds <+ suit2Hand Clubs

-- B3 --
-- | Draws a card from deck and adds to hand
--   returns both
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty h       = error "draw: The deck is empty."
draw (Add c d) h   = (d, Add c h)

-- B4 --
-- | Returns bank hand after playing the game accordingly with the given deck
playBank :: Hand -> Hand
playBank deck = playBankHelper deck Empty

playBankHelper deck hand
          | value hand >= 16     = hand
          | otherwise            = playBankHelper smallerDeck biggerHand
  where (smallerDeck,biggerHand) = draw deck hand

-- B5 --
-- | Returns shuffled deck, (shuffles using helper functions)
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck g h = shuffleDeck' g h Empty

shuffleDeck' g deck newDeck
          | deck == Empty          = newDeck
          | otherwise              = shuffleDeck' g1 oldDeck (Add card newDeck)
  where (rInt, g1)      = randomR (1, (size deck)) g
        (card, oldDeck) = nthCard deck rInt


-- Helper function, removes the nth Card
-- (returns removed card and the new deck)
nthCard :: Hand -> Integer -> (Card, Hand)
nthCard h nth = nthCard' h Empty nth

nthCard' (Add c h) h1 nth
          | nth > size (Add c h)   = error "nth is out of bound"
          | nth == 1               = (c, (h<+h1))
          | otherwise              = nthCard' deck checkedHand (nth - 1)
  where (deck, checkedHand) = draw (Add c h) h1

-- Helper function, checks if card is in hand
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

-- Tests (4 proper shuffle)
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffleDeck g h
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h =
    size h == size (shuffleDeck g h)

-- B6 --
-- | Interface Implementation
implementation = Interface
  { iFullDeck = fullDeck
  , iValue    = value
  , iDisplay  = display
  , iGameOver = gameOver
  , iWinner   = winner
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffleDeck
  }

main :: IO ()
main = runGame implementation




