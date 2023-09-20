module Geometry (
    -- Import the type and the constructors
    Point(..),
    Shape(..),
    area
) where

data Point = Point { x :: Float, y :: Float } deriving (Show)
data Shape = Circle { center :: Point, radius :: Float }

area :: Shape -> Float
area (Circle _ r) = pi * r^2
