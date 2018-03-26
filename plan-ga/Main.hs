{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Graphics.Gloss
import           Test.QuickCheck

data Task = Task
    { duration     :: Float -- in time units
    , resourceCost :: Float -- in resource units
    }
    deriving Show

instance Arbitrary Task where
    arbitrary = do
        Positive duration <- arbitrary
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
    drawWork Work{task = Task{duration}, startTime} =
        translate startTime 0
        . color (makeColor 0 0 1 0.1)
        $ rectangleSolid duration workBlockHeight

main :: IO ()
main = do
    tasks :: [Task] <- generate arbitrary
    putStr "tasks = "; print tasks
    let solution0 = [Work{task, startTime = 0} | task <- tasks]
    display' solution0
  where
    display' schedule = display window white $ translate dx dy pic
      where
        window = InWindow title (round width, round height) (0, 0)
        pic = draw schedule
        title = "Planning with genetic algorithm"
        height = workBlockHeight
        width = maximum
            [ startTime + duration
            | Work{task = Task{duration}, startTime} <- schedule
            ]
        dx = - width / 2
        dy = 0

workBlockHeight :: Float
workBlockHeight = 100
