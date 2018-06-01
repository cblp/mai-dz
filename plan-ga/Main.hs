import           AI.GeneticAlgorithm.Simple
import           Bookkeeper
import           Bookkeeper.Lens ()
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
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

-- | List of chains
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
runPlan Env{envChains} plan =
    evalState ?? start $ do
        for_ plan go
        finalize
        schedule    <- use #schedule
        maxResource <- use #maxResource
        pure (schedule, maxResource)

  where
    start = emptyBook
        & #chains      =: envChains
        & #jobs        =: Map.empty
        & #time        =: 0
        & #schedule    =: Map.empty
        & #maxResource =: 0

    go Wait = do
        jobs <- uses #jobs Map.minViewWithKey
        case jobs of
            Nothing                -> pure ()
            Just ((end, _), jobs') -> do
                #time .= end
                #jobs .= jobs'

    go (Run ch) = do
        chains <- use #chains
        unless (null chains) $ do
            let (work@Work{chainId}, chains') = popWork ch chains
            chainIsIdle <- checkChainIdle chainId
            when chainIsIdle $ do
                #chains .= chains'
                addAndCheck work

    finalize = do
        chains <- use #chains
        for_ (concat chains) addAndCheck

    addAndCheck work@Work{duration} = do
        time <- use #time
        let end = time + duration
        #schedule .@ time <>= Set.singleton work
        #jobs     .@ end  <>= Set.singleton work
        checkResources

    checkChainIdle ch = do
        jobs <- use #jobs
        pure
            (null
                [ ()
                | works <- toList jobs
                , Work{chainId} <- toList works
                , chainId == ch
                ])

    checkResources = do
        jobs <- use #jobs
        let resource = sum
                [ resourceCost
                | works <- toList jobs
                , Work{resourceCost} <- toList works
                ]
        #maxResource %= max resource

-- | 'at' with 'mempty' as default.
(.@) :: (Ord k, Eq m, Monoid m) => Lens' a (Map k m) -> k -> Lens' a m
focusMap .@ key = focusMap . at key . non mempty

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
    pic = (`foldMap` Map.assocs schedule) $ \(start, works) -> mconcat
        [ translate
            (xscale * (start + duration / 2))
            (- (fromIntegral chainId - 0.5) * workBlockHeight)
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
    chainCount =
        1 +
        maximum
            [chainId | works <- toList schedule, Work{chainId} <- toList works]

workBlockHeight :: Float
workBlockHeight = 200

globalResourceLimit :: Resource
globalResourceLimit = 4

xscale :: Float
xscale = 40

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
    populationSize = 10
    stop _ count = count > 200
