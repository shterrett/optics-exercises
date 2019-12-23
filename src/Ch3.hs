module Ch3 where

import Control.Lens

data Ship = Ship
          { _name :: String
          , _numCrew :: Int
          }
          deriving (Show)

name :: Lens' Ship String
name = lens _name (\s n -> s { _name  = n })

numCrew :: Lens' Ship Int
numCrew = lens _numCrew (\s n -> s { _numCrew = n })

second :: Lens' (a, b, c) b
second = lens (\(_, b, _) -> b) (\(a, _, c) b' -> (a, b', c))

-- inMaybe :: Lens' (Maybe a) a
-- attempting to get a out of a Maybe a might fail

-- left :: Lens' (Either a b) a
-- attempting to retrieve the value inside a Left might fail

-- listThird :: Lens' [a] a
-- The list may only be two elements long

conditional :: Lens' (Bool, a, a) a
conditional =
    lens (\case
          (True, a, _) -> a
          (False, _, a) -> a
         )
         (\(bool, a, a') a'' ->
          if bool
            then (bool, a'', a')
            else (bool, a, a'')
         )

data Err =
  ReallyBadError { _msg :: String }
  | ExitCode { _code :: Int }
-- msg :: Lens' Err String
-- No; this is the same as Either Int String


-- Breaks get-set and/or set-set

data BrokenSet = BrokenSet { _brokenSet :: Maybe String }
brokenSet :: Lens' BrokenSet (Maybe String)
brokenSet = lens _brokenSet (\b _ -> b { _brokenSet = Nothing })

breaksGetSet :: Bool
breaksGetSet = view brokenSet (set brokenSet (Just "hi") (BrokenSet $ Just "bye")) /= Just "hi"

msg :: Lens' Err String
msg = lens getMsg setMsg
  where getMsg (ReallyBadError message) = message
        -- Hrmm, I guess we just return ""?
        getMsg (ExitCode _) = ""
        setMsg (ReallyBadError _) newMessage = ReallyBadError newMessage
        -- Nowhere to set it, I guess we do nothing?
        setMsg (ExitCode n) _ = ExitCode n

msgGetSet :: Bool
msgGetSet = view msg (set msg "hi" $ ExitCode 5) == "hi"
-- False; does not respect get-set

msgSetSet :: Bool
msgSetSet = view msg (set msg "hi" (set msg "hi" $ ExitCode 5)) == view msg (set msg "hi" $ ExitCode 5)
-- True; respects set-set

-- Lens that violates all the laws
data Up = Up Int
        deriving (Eq)

up :: Lens' Up Int
up = lens (\(Up i) -> i) (\(Up i) _ -> Up $ i + 1)

upGetSet :: Bool
upGetSet = view up (set up 5 $ Up 2) == 5
-- False

upSetSet :: Bool
upSetSet = set up 5 (set up 5 $ Up 6) == Up 5
-- False

upSetGet :: Bool
upSetGet = set up (view up $ Up 1) (Up 1) == Up 1
-- False

-- Lawful lens for
data Builder =
    Builder { _context :: [String]
            , _build :: [String] -> String
            }

instance Eq Builder where
    (Builder c1 b1) == (Builder c2 b2) = b1 c1 == b2 c2

builderString :: Lens' Builder String
builderString = lens getBuilder setBuilder
  where getBuilder (Builder c b) = b c
        setBuilder (Builder c _) s = Builder c (\_ -> s)

builderGetSet :: Bool
builderGetSet =
    let
      builder = Builder [] head
    in
      view builderString (set builderString "hello" builder) == "hello"

builderSetSet :: Bool
builderSetSet =
    let
      builder = Builder [] head
      helloBuilder = Builder [] (const "hello")
    in
      set builderString "hello" (set builderString "hello" builder) == helloBuilder

builderSetGet :: Bool
builderSetGet =
    let
      helloBuilder = Builder [] (const "hello")
    in
      set builderString (view builderString helloBuilder) helloBuilder == helloBuilder

data User =
    User { _firstName :: String
         , _lastName :: String
         , _email :: String
         }
makeLenses ''User

username :: Lens' User String
username = lens _email (\u uname -> u { _email = uname })

fullName :: Lens' User String
fullName = lens getFullName setFullName
  where getFullName u = _firstName u <> " " <> _lastName u
        setFullName u fname =
          let
            ns = words fname
          in
            u { _firstName = mconcat $ take 1 ns
              , _lastName = mconcat $ drop 1 ns
              }

data ProducePrices =
    ProducePrices { _limePrice :: Float
                  , _lemonPrice :: Float
                  }
                  deriving Show

limePrice :: Lens' ProducePrices Float
limePrice = lens getLimePrice setLimePrice
  where getLimePrice = _limePrice
        setLimePrice p lp = p { _limePrice = max 0 lp }

lemonPrice :: Lens' ProducePrices Float
lemonPrice = lens getLemonPrice setLemonPrice
  where getLemonPrice = _lemonPrice
        setLemonPrice p lp = p { _lemonPrice = max 0 lp }

rLimePrice :: Lens' ProducePrices Float
rLimePrice = lens getLimePrice setLimePrice
  where getLimePrice = _limePrice
        setLimePrice p lp = p { _limePrice = lp
                              , _lemonPrice = withinFiftyCents lp (_lemonPrice p)
                              }

rLemonPrice :: Lens' ProducePrices Float
rLemonPrice = lens getLemonPrice setLemonPrice
  where getLemonPrice = _lemonPrice
        setLemonPrice p lp = p { _lemonPrice = lp
                               , _limePrice = withinFiftyCents lp (_limePrice p)
                               }

withinFiftyCents :: Float -> Float -> Float
withinFiftyCents target given | given - target < -0.50 = target - 0.50
                              | given - target > 0.50 = target + 0.50
                              | otherwise = given
