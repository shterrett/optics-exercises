module Ch4 where

import Control.Lens

-- vorp :: Lens (Vorpal x) (Vorpal y) x y

data Preferences a b =
    Preferences { _best :: a
                , _worst :: b
                }

best :: Lens (Preferences a x) (Preferences b x) a b
best = lens getter setter
  where getter = _best
        setter ps b = ps { _best = b }

worst :: Lens (Preferences x a) (Preferences x b) a b
worst = lens getter setter
  where getter = _worst
        setter ps b = ps { _worst = b }

data Result e =
    Result { _lineNumber :: Int
           , _result :: Either e String
           }

result :: Lens (Result a) (Result b) (Either a String) (Either b String)
result = lens getter setter
  where getter = _result
        setter rs e = rs { _result = e }

-- change more than one type variable at a time
-- sure, but they have to be in the same focus
-- to wit
data Result' e s =
    Result' { _lineNumber' :: Int
            , _result' :: Either e s
            }

result' :: Lens (Result' a c) (Result' b d) (Either a c) (Either b d)
result' = lens getter setter
  where getter = _result'
        setter rs e = rs { _result' = e }

data Predicate a = Predicate (a -> Bool)

predicate :: Lens (Predicate a) (Predicate b) (a -> Bool) (b -> Bool)
predicate = lens getter setter
  where getter (Predicate f) = f
        setter _ b = Predicate b

wizardName :: Bool
wizardName = view (_2 . _1 . _2) ("Ginerva", (("Galileo", "Waldo"), "Malfoy")) == "Waldo"
