module Ch7 where

import Control.Lens

data User =
    User { _name :: String
         , _age :: Int
         } deriving Show
makeLenses ''User

data Account =
    Account { _id :: String
            , _user :: User
            } deriving Show
makeLenses ''Account

mkAccountAge :: Int -> Account
mkAccountAge a = Account "id" (User "name" a)

validateAge :: Account -> Either String Account
validateAge acct = acct & (user . age) %%~ validAge
  where validAge a | a < 0 = Left "Age must be greater than zero"
                   | a > 150 = Left "Age must be less than 150"
                   | otherwise = Right a

data Transaction =
    Withdrawal {_amount :: Int}
    | Deposit {_amount :: Int}
    deriving Show
makeLenses ''Transaction

newtype BankAccount =
    BankAccount
    { _transactions :: [Transaction] }
    deriving Show
makeLenses ''BankAccount

--          Traversal [Transaction] [Transaction] Int Int
--          Applicative f => (Int -> f Int) -> [Transaction] -> f [Transaction]
deposits :: Traversal' [Transaction] Int
deposits _ [] = pure []
deposits f (w@(Withdrawal _) : ts) = (:) <$> pure w <*> deposits f ts
deposits f (Deposit d : ts) = (:) <$> (Deposit <$> f d) <*> deposits f ts

deposits' :: Traversal' [Transaction] Int
deposits' = traversed . filtered isDeposit . amount
  where isDeposit (Deposit _) = True
        isDeposit (Withdrawal _) = False

--         (Int -> f Int) -> Transaction -> f Transaction
amountT :: Traversal' Transaction Int
amountT f (Withdrawal amt) = Withdrawal <$> f amt
amountT f (Deposit amt) = Deposit <$> f amt

--      (a -> f b) -> (a, a) -> f (b, b)
both :: Traversal (a, a) (b, b) a b
both f (a1, a2) = (,) <$> f a1 <*> f a2

--                  (Int -> f Int) -> Transaction -> f Transaction
transactionDelta :: Traversal' Transaction Int
transactionDelta f (Withdrawal amt) = Withdrawal . ((-1) *) <$> f (-1 * amt)
transactionDelta f (Deposit amt) = Deposit <$> f amt

--      (a -> f a') -> Either a b -> f (Either a' b)
left :: Traversal (Either a b) (Either a' b) a a'
left _ (Right b) = pure (Right b)
left f (Left a) = Left <$> f a

beside :: Traversal s t a b -- (a -> f b) -> s -> f t
       -> Traversal s' t' a b -- (a -> f b) -> s' -> f t'
       -> Traversal (s,s') (t,t') a b -- (a -> f b) -> (s, s') -> f (t, t')
beside tr tr' f (s, s') = (,) <$> (traverseOf tr f s) <*> (traverseOf tr' f s')
