{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import           AI.GeneticAlgorithm.Simple
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.State
import           Data.Traversable
import           GHC.Generics
import           Graphics.Gloss
import           System.Random

data Task = Task
    { duration     :: Float -- in time units
    , resourceCost :: Float -- in resource units
    }
    deriving (Eq, Generic, NFData, Ord, Show)

data Work = Work
    { task      :: Task
    , startTime :: Float
    }
    deriving (Eq, Generic, NFData, Ord, Show)

type Schedule = [Work]

randomTask :: State StdGen Task
randomTask = do
    duration <- exp <$> randomS
    resourceCost <- exp <$> randomS
    pure Task{..}

randomSchedule :: [Task] -> State StdGen Schedule
randomSchedule = traverse $ \task -> do
    startTime <- randomS
    pure Work{task, startTime}

instance Chromosome Schedule where
    crossover g schedule1 schedule2 = ([schedule], g)
      where
        schedule =
            [ Work{startTime = (st1 + st2) / 2, task}
            | Work{startTime = st1, task} <- schedule1
            | Work{startTime = st2} <- schedule2
            ]

    mutation g schedule = (`runState` g) $
        for schedule $ \work@Work{startTime} -> do
            multiplier <- exp <$> randomS
            let startTime' = multiplier * startTime
            pure work{startTime = startTime'}

    fitness schedule = _

draw :: Schedule -> Picture
draw = foldMap drawWork
  where
    drawWork Work{task = Task{duration}, startTime} =
        translate startTime 0
        . color (makeColor 0 0 1 0.1)
        $ rectangleSolid duration workBlockHeight

main :: IO ()
main = do
    g <- newStdGen

    let tasks = evalState (replicateM 10 randomTask) g
    putStrLn $ "tasks = " ++ show tasks
    -- let solution0 = [Work{task, startTime = 0} | task <- tasks]
    -- display' solution0

    let solution = runGA g populationSize 0.1 (runState $ randomSchedule tasks) stop
    display' solution

  where
    populationSize = 10
    stop _ count = count > 10

display' :: Schedule -> IO ()
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

randomS :: (Random a, RandomGen g) => State g a
randomS = state random
