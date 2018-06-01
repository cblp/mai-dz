import           Prelude hiding (read)

import           AI.GeneticAlgorithm.Simple
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.STRef.Extra
import           Data.Traversable
import           GHC.Generics
import           Graphics.Gloss
import           System.Random

type Time = Float

type WorkId = String

type Resource = Float

data Work = Work
    { workId       :: WorkId
    , duration     :: Time
    , resourceCost :: Resource
    , chainId      :: ChainId
    }
    deriving (Eq, Generic, NFData, Ord, Show)

type Chain = [Work]

type Chains = [Chain]

type ChainId = Int

data Step = Wait | Run ChainId
    deriving (Generic, NFData, Show)

type Plan = [Step]

data Env = Env
    { envChains        :: Chains
    , envResourceLimit :: Resource
    }
    deriving (Generic, NFData)

type Schedule = Map Time (Set Work)

randomWork :: WorkId -> ChainId -> State StdGen Work
randomWork workId chainId = do
    duration <- randomRS (1, 10)
    resourceCost <- randomRS (1, 4)
    pure Work{..}

scheduleEndTime :: Schedule -> Time
scheduleEndTime schedule
    | null schedule = 0
    | otherwise     = maximum
        [ start + duration
        | (start, works) <- Map.assocs schedule, Work{duration} <- toList works
        ]

randomPlan :: Chains -> State StdGen Plan
randomPlan chains = case chains of
    [] -> pure []
    _  -> do
        b <- randomRS (False, True)
        if b then do
            i <- randomS
            let (_, chains') = popWork i chains
            (Run i :) <$> randomPlan chains'
        else
            (Wait :) <$> randomPlan chains

popWork :: Int -> Chains -> (Work, Chains)
popWork ch chains =
    case chains !! i of
        []         -> error "empty chain"
        [work]     -> (work, take i chains ++         drop (i + 1) chains)
        work:chain -> (work, take i chains ++ chain : drop (i + 1) chains)
  where
    i = ch `mod` length chains

twist :: [a] -> [a] -> [a]
twist []     ys = ys
twist (x:xs) ys = x : twist ys xs

instance Chromosome (Env, Plan) where
    crossover g (env, plan1) (_, plan2) =
        (map (env,) [plan1, plan2, twist plan1 plan2, twist plan2 plan1], g)

    mutation g (env, plan) = (runState ?? g) . fmap (env,) $ do
        i <- randomRS (0, length plan - 1)
        d <- randomRS (False, True)
        if d then do
            -- insert step
            b <- randomRS (False, True)
            step <- if b then Run <$> randomS else pure Wait
            pure (take i plan ++ step : drop i plan)
        else
            -- remove step
            pure (take i plan ++ drop (i + 1) plan)

    fitness (env, plan)
        | maxResourceCost <= envResourceLimit =
            1
            / realToFrac (scheduleEndTime schedule)
            / fromIntegral (length plan + 1)
        | otherwise =
            - fromIntegral (length plan + 1) * realToFrac maxResourceCost
      where
        (schedule, maxResourceCost) = runPlan env plan
        Env{envResourceLimit} = env

runPlan :: Env -> Plan -> (Schedule, Resource)
runPlan Env{envChains} plan = runST $ do
    chainsR      <- new envChains
    jobsR        <- new Map.empty
    timeR        <- new 0
    scheduleR    <- new Map.empty
    maxResourceR <- new 0

    let checkChainIdle ch =
            all (\Work{chainId} -> chainId /= ch) . fold <$> read jobsR

    let checkResources = do
            jobs <- read jobsR
            let resource = sum
                    [ resourceCost
                    | works <- toList jobs
                    , Work{resourceCost} <- toList works
                    ]
            maxResourceR $~ max resource

    let addAndCheck work@Work{duration} = do
            time <- read timeR
            let end = time + duration
            scheduleR $~ atd time %~ Set.insert work
            jobsR     $~ atd end  %~ Set.insert work
            checkResources

    let go Wait = do
            jobs <- read jobsR
            case Map.minViewWithKey jobs of
                Nothing                -> pure ()
                Just ((end, _), jobs') -> do
                    timeR $= end
                    jobsR $= jobs'

        go (Run ch) = do
            chains <- read chainsR
            unless (null chains) $ do
                let (work@Work{chainId}, chains') = popWork ch chains
                chainIsIdle <- checkChainIdle chainId
                when chainIsIdle $ do
                    chainsR $= chains'
                    addAndCheck work

    let finalize = do
            chains <- read chainsR
            for_ (concat chains) addAndCheck

    for_ plan go
    finalize
    schedule    <- read scheduleR
    maxResource <- read maxResourceR
    pure (schedule, maxResource)

  where
    atd key = at key . non mempty

display' :: Schedule -> IO ()
display' schedule = display window white (translate dx dy pic)
  where
    window = InWindow title size (0, 0)
    (pic, size@(width, _)) = drawSchedule schedule
    title = "Planning with genetic algorithm"
    dx = - fromIntegral width / 2
    dy = 0

drawSchedule :: Schedule -> (Picture, (Int, Int))
drawSchedule schedule =
    (pic, (round width, round (workBlockHeight * fromIntegral chainCount)))
  where
    pic = foldMap ?? Map.assocs schedule $ \(start, works) -> mconcat
        [ translate
            (xscale * (start + duration / 2))
            (   - (fromIntegral chainId - fromIntegral maxChainId / 2)
            *   workBlockHeight
            )
            (   scale xscale 1
                    (   color translucentBlue (rectangleSolid w h)
                    <>  rectangleWire duration workBlockHeight
                    )
            <>  scale fontSize fontSize (text workId)
            )
        | Work{duration, workId, chainId, resourceCost} <- toList works
        , let
            w = duration
            h = workBlockHeight * resourceCost / globalResourceLimit
        ]
    width = xscale * scheduleEndTime schedule
    translucentBlue = makeColor 0 0 1 0.33
    fontSize = 0.1
    maxChainId =
        maximum
            [chainId | works <- toList schedule, Work{chainId} <- toList works]
    chainCount = 1 + maxChainId

workBlockHeight :: Float
workBlockHeight = 200

globalResourceLimit :: Resource
globalResourceLimit = 4

xscale :: Float
xscale = 30

randomS :: (Random a, RandomGen g) => State g a
randomS = state random

randomRS :: (Random a, RandomGen g) => (a, a) -> State g a
randomRS = state . randomR

main :: IO ()
main = do
    gen <- newStdGen

    let chains =
            if generateRandomChains then
                evalState ?? (0 :: Int, gen) $
                for [0..1] $ \chainId ->
                replicateM 3 $ do
                    n <- _1 <<+= 1
                    zoom _2 (randomWork (show n) chainId)
            else
                [ [Work "1" 3 2 0, Work "3" 2 4 0, Work "5" 1 3 0]
                , [Work "2" 4 3 1, Work "4" 2 4 1, Work "6" 4 2 1]
                ]

    let env = Env{envChains = chains, envResourceLimit = globalResourceLimit}
    putStrLn "chains ="
    for_ chains $ \chain -> do
        putStr "\t"
        print chain
    let newChromosome g = ((env, plan), g') where
            (plan, g') = runState (randomPlan chains) g

    -- solution
    let (_, plan) =
            runGA gen populationSize mutationChance newChromosome stop
    let (schedule, maxResource) = runPlan env plan
    putStrLn ("*** plan = " ++ show plan)
    putStrLn ("*** schedule = " ++ show schedule)
    putStrLn ("*** maxResource = " ++ show maxResource)
    putStrLn ("*** scheduleEndTime = " ++ show (scheduleEndTime schedule))
    display' schedule

  where
    generateRandomChains = False
    mutationChance = 0.5
    populationSize = 20
    stop _ count = count > 200
