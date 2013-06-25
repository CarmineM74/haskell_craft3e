module CardGame where
import Prelude

-- 6.53
data Suit = Spades | Hearts |
            Diamonds | Clubs
            deriving (Eq)

-- 2..9,j,q,k,* (Ace)
type Value = Char

type Card = (Suit,Value)

type Deck = [Card]

-- 6.55
data Player = North | South |
              East | West

              deriving (Show)

-- 6.56
-- Head leads
type Trick = [(Player,Card)]

noTrumpTrick    :: Trick
noTrumpTrick = [(East,(Hearts,'j')),(South,(Hearts,'3')),(West,(Hearts,'2')),(North,(Hearts,'k'))]

trickWithTrump  :: Trick
trickWithTrump = [ (North, (Diamonds, '3')), (South,(Clubs,'*')), (East,(Diamonds,'7')), (West,(Diamonds,'9')) ]

-- 6.57
-- Assumes no trump
winNT         :: Trick -> Player
winNT trick = head [ p | (p,v) <- player_values, v == max_value]
  where
    max_value = maximum [ v | (p,(s,v)) <- trick ]
    player_values = [ (p,v) | (p,(s,v)) <- trick ]

-- 6.58
-- Assumes at least one trump exists
winT'          :: Trick -> Player
winT' trick = head [ p | (p,(s,v)) <- trumps, v == max_trump_value ]
  where
    (p,(lead_suit,v)) = head trick
    trumps = [ (p,(s,v)) | (p,(s,v)) <- trick, s /= lead_suit ]
    max_trump_value = maximum [ v | (p,(s,v)) <- trumps ]

winT            :: Suit -> Trick -> Player
winT trump trick = winNT trumps
  where
    trumps = [ (p,(s,v)) | (p,(s,v)) <- trick, s == trump ]
