module Ch5 where

import Control.Lens
import Data.Char (toUpper)

data Gate =
    Gate { _open :: Bool
         , _oilTemp :: Float
         } deriving (Show, Eq)
makeLenses ''Gate

data Army =
    Army { _archers :: Int
         , _knights :: Int
         } deriving (Show, Eq)
makeLenses ''Army

data Kingdom =
    Kingdom { _name :: String
            ,_army :: Army
            ,_gate :: Gate
            } deriving (Show, Eq)
makeLenses ''Kingdom

duloc :: Kingdom
duloc =
    Kingdom
    { _name = "Duloc"
    , _army =
      Army
      { _archers = 22
      , _knights = 14
      }
    , _gate =
      Gate
      { _open = True
      , _oilTemp = 10.0
      }
    }

goalA :: Kingdom
goalA =
    Kingdom
    { _name = "Duloc: a perfect place"
    , _army =
      Army
      { _archers = 22
      , _knights = 42
      }
    , _gate =
      Gate
      { _open = False
      , _oilTemp = 10.0
      }
    }

pathA :: Kingdom
pathA =
  duloc
    & name .~ "Duloc: a perfect place"
    & army . knights .~ 42
    & gate . open .~ False

pathAHard :: Kingdom
pathAHard =
  duloc
    & name <>~ ": a perfect place"
    & army . knights +~ 28
    & gate . open &&~ False

goalB :: Kingdom
goalB = Kingdom
  { _name = "Dulocinstein"
  , _army =
    Army
    { _archers = 17
    , _knights = 26
    }
  , _gate = Gate
    { _open = True
    , _oilTemp = 100.0
    }
  }

pathB :: Kingdom
pathB =
    duloc
      & name .~ "Dulocinstein"
      & army . archers -~ 5
      & army . knights +~ 12
      & gate . oilTemp *~ 10

pathBHard :: Kingdom
pathBHard =
    duloc
      & name <>~ "instein"
      & army . archers -~ 5
      & army . knights +~ 12
      & gate . oilTemp *~ 10

goalC :: (String, Kingdom)
goalC = ( "Duloc: Home"
        , Kingdom
          { _name = "Duloc: Home of the talking Donkeys"
          , _army =
            Army
            { _archers = 22
            , _knights = 14
            }
          , _gate =
            Gate
            { _open = True
            , _oilTemp = 5.0
            }
          }
        )

pathC :: (String, Kingdom)
pathC =
    duloc
      & gate . oilTemp .~ 5.0
      & name <>~ ": Home"
      & name <<.~ "Duloc: Home of the talking Donkeys"

pathCHard :: (String, Kingdom)
pathCHard =
    duloc
      & gate . oilTemp -~ 5
      & name <>~ ": Home"
      & name <<<>~ " of the talking Donkeys"

undefined1 :: (Bool, String)
undefined1 = ((False, "opossums") & _1 ||~ True)

undefined1T :: (Bool, String)
undefined1T = (True, "opossums")

undefined2 :: ((Bool, String), Float)
undefined2 =
    ((True, "Dudley"), 55.0)
      & _1 . _2 <>~ " - the worst"
      & _2 -~ 15
      & _2 //~ 2
      & _1 . _2 %~ map toUpper
      & _1 . _1 .~ False

undefined2T :: ((Bool, String), Float)
undefined2T = ((False, "DUDLEY - THE WORST"), 20.0)
