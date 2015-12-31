data Suit = Club | Diamond | Heart | Spade
    deriving (Show, Eq, Enum)

data Rank = Rank Int

instance Show Rank where
   show (Rank 1) = "Ace"
   show (Rank 11) = "Jack"
   show (Rank 12) = "Queen"
   show (Rank 13) = "King"
   show (Rank i) = show i

deck = [(Rank rank, suit) | rank <- [1..13], suit <- [Club .. Spade]]

main = print deck
