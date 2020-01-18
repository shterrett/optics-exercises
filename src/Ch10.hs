module Ch10 where

import Control.Lens

celsiusToF :: Double -> Double
celsiusToF c = (c * (9/5)) + 32

fahrenheitToC :: Double -> Double
fahrenheitToC f = (f - 32) * (5/9)

fahrenheit :: Iso' Double Double
fahrenheit = iso celsiusToF fahrenheitToC

intNot :: Int -> Int
intNot = not ^. dimapping enum (from enum)

intNot' :: Int -> Int
intNot' = enum %~ not
