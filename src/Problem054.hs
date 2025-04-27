import Data.List (sort)


data Type = Hearts | Diamonds | Clubs | Spades deriving (Eq,Show)
data Card = Card {suit::Type, rank::Int} deriving(Eq,Show)
data Hand = Hand {cards::[Card]} deriving (Show)
values hand = map (rank) (cards hand)
suits hand = map (suit) (cards hand)

instance Ord Card  where
    (<=) (Card s1 r1) (Card s2 r2) =  r1 <= r2

data Rank = HighCard Int | OnePair Int | TwoPairs Int Int | ThreeK Int
    | Straight Card Card Card Card Card | Fullhouse (Int) (Int) | FourK Int
    | Flush Card | SFlush Type | RFlush Type deriving (Show)

consecutives xs = aux xs
    where
        aux [] = True
        aux [a] = True
        aux [a,b] = abs (b-a) == 1
        aux (a:b:c) = if abs (b-a) == 1 then aux (b:c) else False


sameSuit:: Hand -> Bool
sameSuit hand = 
    let firstSuit = suit (head (cards hand))
        in all (==firstSuit) (suits hand)



getRank :: Hand -> Rank
getRank hand
    | sameSuit hand && sort (values hand) == [10,11,12,13,14] = RFlush (suit $ head $ cards hand)
    | sameSuit hand && consecutives (values hand) = SFlush (suit $ head $ cards hand)
    | all (==(head $ values hand)) (values hand) 
    | otherwise = HighCard 1

