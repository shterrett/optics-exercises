module Ch8 where

import Control.Lens
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T

newtype CiMap v = CiMap { unCiMap :: Map Text v }
                  deriving (Show)

type instance Index (CiMap v) = Text
type instance IxValue (CiMap v) = v

instance Ixed (CiMap v) where
    ix k f (CiMap m) = CiMap <$> (ix (T.toLower k) f m)

instance At (CiMap v) where
    at k f (CiMap m) = CiMap <$> (at (T.toLower k) f m)
