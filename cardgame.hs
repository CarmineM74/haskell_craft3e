module CardGame where
import Prelude

-- 6.53
data Suit = Spades | Hearts |
            Diamonds | Clubs
            deriving (Eq, Show)

-- 2..9,j,q,k,* (Ace)
type Value = Char

type Card = (Suit,Value)

type Deck = [Card]

-- 6.55
data Player = North | South |
              East | West
              deriving (Eq,Show)

-- 6.56
-- Head leads
type Trick = [(Player,Card)]

noTrumpTrick    :: Trick
noTrumpTrick = [(East,(Spades,'3')),(South,(Spades,'k')),(West,(Spades,'6')),(North,(Spades,'*'))]

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

-- 6.59
type Hand = (Player,[Card])

-- 6.60
type Hands = [Hand]

north   :: Hand
north = (North, [(Spades,'*'),(Hearts,'k'),(Hearts,'7'),(Diamonds,'3')])

south   :: Hand
south = (South, [(Spades,'k'),(Spades,'j'),(Spades,'q'),(Clubs,'*'),(Hearts,'2')])

east    :: Hand
east = (East,[(Spades,'3'),(Hearts,'j'),(Diamonds,'7'),(Diamonds,'2'),(Clubs,'9')])

west    :: Hand
west = (West,[(Spades,'6'),(Diamonds,'9'),(Diamonds,'6'),(Clubs,'k'),(Clubs,'6')])

sampleHands     :: Hands
sampleHands = [north,south,east,west]

-- 6.61
-- Given the players' hands, and a trick
-- it checks wheter the trick is both possible and legal or not
-- A trick is possible when the card played by each player is in theri hand
-- A trick is legal when the player follow the leading suit if they can
--

playerHasCard     :: Player -> Hands -> Card -> Bool
playerHasCard p hs c = and [elem c h | (player,h) <- hs, player == p]

playerHasSuit     :: Player -> Hands -> Suit -> Bool
playerHasSuit p hs s = elem s player_suits
  where
    player_suits = fst $ unzip $ concat [h | (player,h) <- hs, player == p]

checkPlay     :: Hands -> Trick -> Bool
checkPlay hs t = isPossible && isLegal
  where
    isPossible =  and [ True | (p,c) <- t, playerHasCard p hs c ]
    isLegal = length (can_follow ++ cant_follow) == 4
    (lead_player,(lead_suit,_)) = head t 
    can_follow = [ p | (p,(s,v)) <- t, s == lead_suit, playerHasCard p hs (s,v) ]
    cant_follow = [ p | (p,(s,v)) <- t, s /= lead_suit, not (playerHasSuit p hs lead_suit)]
    
