module Ch9 where

import Control.Lens

_Just' :: Prism (Maybe a) (Maybe b) a b
_Just' = prism embed match
  where embed :: b -> Maybe b
        embed = Just
        match :: Maybe a -> Either (Maybe b) a
        match (Just a) = Right a
        match Nothing = Left Nothing

_Right' :: Prism (Either x a) (Either x b) a b
_Right' = prism embed match
  where embed :: b -> Either x b
        embed = Right
        match :: Either x a -> Either (Either x b) a
        match (Left x) = Left (Left x)
        match (Right a) = Right a

-- Can't implement as Prism' [a] [a] because when trying to embed the tail,
-- there's no head of the list to put it behind.
_Tail :: Prism' [a] (a, [a])
_Tail = prism' embed match
  where embed :: (a, [a]) -> [a]
        embed (h, t) = h : t
        match :: [a] -> Maybe (a, [a])
        match [] = Nothing
        match (a:as) = Just (a, as)

_ListCons :: Prism [a] [b] (a, [a]) (b, [b])
_ListCons = prism embed match
  where embed :: (a, [a]) -> [a]
        embed = uncurry (:)
        match :: [a] -> Either [b] (a, [a])
        match [] = Left []
        match (a:as) = Right (a, as)

_Cycles :: Eq a => Int -> Prism [a] [b] [a] [b]
_Cycles n = prism embed match
  where embed :: [a] -> [a]
        embed atom = mconcat $ replicate n atom
        match :: Eq a => [a] -> Either [b] [a]
        match as =
          let
            atomLength = length as `div` n
          in
            case go atomLength (take atomLength as) (drop atomLength as) of
              Just atom -> Right atom
              Nothing -> Left []
        go :: Eq a => Int -> [a] -> [a] -> Maybe [a]
        go _ atom [] = Just atom
        go atomLength atom rest =
          if take atomLength rest == atom
            then go atomLength atom (drop atomLength rest)
            else Nothing
