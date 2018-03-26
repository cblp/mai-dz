{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Graphics.Gloss
import           Test.QuickCheck

data Task = Task
    { timeCost     :: Float -- time cost, in time units
    , resourceCost :: Float -- resource cost, in resource units
    }
    deriving Show

instance Arbitrary Task where
    arbitrary = do
        Positive timeCost <- arbitrary
        Positive resourceCost <- arbitrary
        pure Task{..}

type Schedule = [Work]

data Work = Work
    { task      :: Task
    , startTime :: Float
    }

draw :: Schedule -> Picture
draw = foldMap drawWork
  where
    drawWork Work{task = Task{timeCost}, startTime} =
        translate startTime 0
        . color (makeColor 0 0 1 0.1)
        $ rectangleSolid timeCost 100

main :: IO ()
main = do
    tasks :: [Task] <- generate arbitrary
    putStr "tasks = "; print tasks
    let solution0 = [Work{task, startTime = 0} | task <- tasks]
    display' solution0
  where
    display' =
        display
            (InWindow "Planning with genetic algorithm" (800, 600) (0, 0))
            white
        . draw
