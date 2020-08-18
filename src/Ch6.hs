module Ch6 where

import Control.Lens
import Control.Monad (guard)
import Data.Ord (comparing)
import Data.Monoid (Sum(..))

p1i :: [String]
p1i = ["umbrella", "olives", "racecar", "hammer"]

p1o :: Maybe String
p1o = Just "racecar"

p1 :: Maybe String
p1 = findOf folded palindrome p1i
  where palindrome w = w == reverse w

p2i :: (Int, Int, Int)
p2i = (2, 4, 6)

p2o :: Bool
p2o = True

p2 :: Bool
p2 = allOf each even p2i

p3i :: [(Int, String)]
p3i = [(2, "I'll"), (3, "Be"), (1, "Back")]

p3o :: Maybe (Int, String)
p3o = Just (3,"Be")

p3 :: Maybe (Int, String)
p3 = maximumByOf folded (comparing fst) p3i

p4i :: (Int, Int)
p4i = (1, 2)

p4o :: Int
p4o = 3

p4 :: Int
p4 = sumOf each p4i

p5i :: String
p5i = "Do or do not, there is no try."

p5o :: Maybe String
p5o = Just "there"

p5 :: Maybe String
p5 = maximumByOf (folding words) (comparing vowels) p5i
  where vowels = foldMap (\case
                            'a' -> Sum (1 :: Int)
                            'e' -> Sum (1 :: Int)
                            'i' -> Sum (1 :: Int)
                            'o' -> Sum (1 :: Int)
                            'u' -> Sum (1 :: Int)
                            _ -> mempty
                         )

p6i :: [String]
p6i = ["a", "b", "c"]

p6o :: String
p6o = "cba"

p6 :: String
p6 = foldOf (folding reverse) p6i

p7i :: [(Int, Int, Int)]
p7i = [(12, 45, 66), (91, 123, 87)]

p7o :: String
p7o = "54321"

p7 :: String
p7 = p7i ^.. folded . _2 . folding (reverse . show)

p8i :: [(Int, String)]
p8i = [(1, "a"), (2, "b"), (3, "c"), (4, "d")]

p8o :: [String]
p8o = ["b", "d"]

p8 :: [String]
p8 = p8i ^.. folded . filtered (\(i, _) -> even i) . _2

sample :: [Int]
sample = [-10, -5, 4, 3, 8, 6, -2, 3, -5, -7]

daysToFirstThaw :: Int
daysToFirstThaw = lengthOf (takingWhile (< 0) folded) sample

warmestInFirstFourDays :: Maybe Int
warmestInFirstFourDays = maximumOf (taking 4 folded) sample

dayAfterWarmestDay :: Maybe Int
dayAfterWarmestDay = sample ^? dropping 1 (droppingWhile (/= targetTemp) folded)
  where targetTemp :: Int
        targetTemp = maybe 0 id warmestInFirstFourDays

freezingAtEnd :: Int
freezingAtEnd = lengthOf (takingWhile (< 0) (backwards folded)) sample

firstThawToNextFreeze :: [Int]
firstThawToNextFreeze = sample ^.. takingWhile (>= 0) (droppingWhile (< 0) folded)

firstThawToLastFreeze :: [Int]
firstThawToLastFreeze = sample ^..  backwards
                                    (droppingWhile (< 0)
                                    (backwards
                                    (droppingWhile (< 0) folded)))

trimmingWhile :: (a -> Bool) -> Fold s a -> Fold s a
trimmingWhile p f = backwards
                    (droppingWhile p
                    (backwards
                    (droppingWhile p f)))

data Card =
    Card
    { _name :: String
    , _aura :: Aura
    , _holo :: Bool
    , _moves :: [Move]
    } deriving (Show, Eq)

data Aura = Wet | Hot | Spark | Leafy
  deriving (Show, Eq)

data Move =
    Move
    { _moveName :: String
    , _movePower :: Int
    } deriving (Show, Eq)

makeLenses ''Card
makeLenses ''Move

deck :: [Card]
deck =
    [ Card "Skwortul" Wet False [Move "Squirt" 20]
    , Card "Scorchander" Hot False [Move "Scorch" 20]
    , Card "Seedasaur" Leafy False [Move "Allergize" 20]
    , Card "Kapichu" Spark False [Move "Poke" 10 , Move "Zap" 30]
    , Card "Elecdude" Spark False [Move "Asplode" 50]
    , Card "Garydose" Wet True [Move "Gary's move" 40]
    , Card "Moisteon" Wet False [Move "Soggy" 3]
    , Card "Grasseon" Leafy False [Move "Leaf Cut" 30]
    , Card "Spicyeon" Hot False [Move "Capsaicisize" 40]
    , Card "Sparkeon" Spark True [Move "Shock" 40 , Move "Battery" 50]
    ]

-- List all the cards whose name starts with 'S'.
startingWithS :: [Card]
startingWithS = deck ^.. folded . filteredBy (name . _head . only 'S')
-- startingWithS = deck ^.. folded
--                        . filtered (\n -> firstOf (name . folded) n == Just 'S')

-- What’s the lowest attack power of all moves?
lowestAttack :: Maybe Move
lowestAttack =
    minimumByOf
      (folded . moves . folded)
      (comparing _movePower)
      deck

-- What’s the name of the first card which has more than one move?
firstMultiMove :: Maybe String
firstMultiMove =
    firstOf
      (folded . filtered (\c -> length (c ^. moves) > 1))
      deck
    ^? _Just . name

-- Are there any Hot cards with a move with more than 30 attack power?
hotAndHeavy :: [Card]
hotAndHeavy =
    deck ^..
      folded
      . filteredBy (aura . only Hot)
      . filtered (anyOf (moves . folded . movePower) (> 30))

-- List the names of all holographic cards with a Wet aura.
holoWet :: [String]
holoWet =
    deck ^..
      folded
      . filteredBy (aura . only Wet)
      . filteredBy (holo . only True)
      . name

-- What’s the sum of all attack power for all moves belonging to non-Leafy cards?
nonLeafyAttackPower :: Int
nonLeafyAttackPower = sumOf (folded
                            . filteredBy (aura . anyBut Leafy)
                            . moves
                            . folded
                            . movePower
                            )
                            deck
-- nonLeafyAttackPower =
--     sumOf
--       (folded . filtered (\c -> c ^. aura /= Leafy) . moves . folded . movePower)
--       deck

anyBut :: (Eq a) => a -> Prism' a ()
anyBut a = prism' (\() -> a) $ guard . (a /=)
