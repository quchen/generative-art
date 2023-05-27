-- | TSP attempts to find a closed loop of minimum length that visits each input
-- point exactly once.
--
-- The general workflow is:
--
--  1. Create a basic data structure with 'inputPath' or 'distances'+'tspNearestNeighbourPath'
--  2. Improve it using 'tsp2optPath', 'tsp3optPath'
--  3. Retrieve the optimized geometry with 'pathToPoints'
module Numerics.Optimization.TSP (
    -- * Setup
      distances
    , Distances
    , pathToPoints
    , TspPoint

    -- * Construction
    , inputPath
    , tspNearestNeighbourPath

    -- * Improvement

    -- ** 2-opt
    , tsp2optPath
    , tsp2optInplace
    , unsafePlan2optSwap
    , unsafeCommit2optSwap

    -- ** 3-opt
    , tsp3optPath
    , tsp3optInplace
    , Case3opt
    , unsafePlan3optSwap
    , unsafeCommit3optSwap

) where



import           Control.Monad.Primitive
import           Data.Coerce
import           Data.Ord
import           Data.Vector             (Vector, (!))
import qualified Data.Vector.Extended    as V
import qualified Data.Vector.Mutable     as VM
import           Geometry.Core
import           System.Random.MWC       as MWC
import           System.Random.Stateful
import           Text.Printf



-- $setup
-- >>> import Draw
-- >>> import Geometry.Algorithms.Sampling
-- >>> import qualified Data.Vector as V
-- >>> import Graphics.Rendering.Cairo as C
-- >>> import qualified System.Random.MWC as MWC



newtype Temperature = T Double deriving (Eq, Ord, Show)
newtype Energy = E Double deriving (Eq, Ord, Show)

-- | Infix shorthand for 'mod' with a fixity of one below addition.
--
-- >>> 7+3 % 4
-- 2
(%) :: Int -> Int -> Int
(%) = mod
infix 5 %

-- | Cache the distance between points i and j.
--
-- The 'Vec2's can be forgotten now, because we’re only interested in topological
-- properties, not the coordinates themselves. Once the calculation is done, the
-- result permutation can be obtained with 'pathToPoints'.
distances :: Vector Vec2 -> Distances
distances points = Distances $
    V.generate (V.length points) $ \i ->
        V.generate i $ \j ->
            norm (points!i -. points!j)

-- | Lookup the distance between two points.
--
-- @
-- 'distance' ('distances' points) i j = 'norm' (points'!'i -. points'!'j)
-- @
distance :: Distances -> TspPoint -> TspPoint -> Double
distance (Distances matrix) (TspPoint i) (TspPoint j) = case compare i j of
    EQ -> 0
    LT -> matrix!j!i
    GT -> matrix!i!j

-- | Cache for the (symmetrical) distance between points. See 'distances'.
newtype Distances = Distances (Vector (Vector Double))

-- | Number of points
size :: Distances -> Int
size (Distances m) = V.length m

instance Show Distances where
    show m@(Distances mxy) = do
        i <- coerce [0 .. V.length mxy-1]
        j <- coerce [0 .. V.length mxy-1]
        if j == TspPoint 0
            then printf "[%6.2f, " (distance m i j)
            else if j >= TspPoint (V.length mxy - 1)
                then printf "%6.2f]\n" (distance m i j)
                else printf "%6.2f, " (distance m i j)

-- | Permute the input vector with the result of a TSP optimization.
pathToPoints :: Vector vec2 -> Vector TspPoint -> Vector vec2
pathToPoints coordinates indices = V.backpermute coordinates (coerce indices)

-- | Interpret the input as a path as-is. The simplest possible TSP solver. :-)
--
-- <<docs/haddock/Numerics/Optimization/TSP/input_path.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Numerics/Optimization/TSP/input_path.svg" 300 200 $ do
--    points <- liftIO $ do
--        gen <- MWC.initialize (V.fromList [])
--        uniform <- uniformlyDistributedPoints gen (shrinkBoundingBox 10 [zero, Vec2 300 200]) 32
--        pure uniform
--    setLineJoin LineJoinRound
--    let path = inputPath points
--        tour = Polygon (V.toList (pathToPoints points path))
--    cairoScope $ do
--        setColor (mathematica97 0)
--        for_ points $ \p -> sketch (Circle p 3) >> fill
--    cairoScope $ do
--        setColor (mathematica97 1)
--        sketch tour
--        stroke
-- :}
-- docs/haddock/Numerics/Optimization/TSP/input_path.svg
inputPath :: Vector a -> Vector TspPoint
inputPath = (coerce :: Vector Int -> Vector TspPoint) . V.enumFromN 0 . V.length

-- | Starting at the \(i\)-th point, create a path by taking the nearest
-- neighbour. This algorithm is very simple and fast, but yields surprisingly short
-- paths for everyday inputs.
--
-- It is an excellent starting point for more accurate but slower algorithms, such
-- as 'tsp2optPath'.
--
-- <<docs/haddock/Numerics/Optimization/TSP/nearest_neighbour_path.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Numerics/Optimization/TSP/nearest_neighbour_path.svg" 300 200 $ do
--    points <- liftIO $ do
--        gen <- MWC.initialize (V.fromList [])
--        uniform <- uniformlyDistributedPoints gen (shrinkBoundingBox 10 [zero, Vec2 300 200]) 32
--        pure uniform
--    setLineJoin LineJoinRound
--    let dm = distances points
--        path = tspNearestNeighbourPath dm
--        tour = Polygon (V.toList (pathToPoints points path))
--    cairoScope $ do
--        setColor (mathematica97 0)
--        for_ points $ \p -> sketch (Circle p 3) >> fill
--    cairoScope $ do
--        setColor (mathematica97 2)
--        sketch tour
--        stroke
-- :}
-- docs/haddock/Numerics/Optimization/TSP/nearest_neighbour_path.svg
tspNearestNeighbourPath :: Distances -> Vector TspPoint
tspNearestNeighbourPath dm = V.create $ do
    let n = size dm
    points <- VM.generate n TspPoint
    let loop vec = do
            result <- nnStep dm vec
            case result of
                Nothing -> pure points
                Just rest -> loop rest
    loop points

-- | Efficient, but less obvious algorithm than the straightforward search. The
-- advantage here is that instead of maintaining a »seen« data structure that we
-- have to traverse fully on each iteration, we only have to search not-yet-seen
-- points by design in this approach.
--
-- 1. Split the input vector in (head, tail). Use the head as the current point.
-- 2. Search the tail for a closer point.
--    If one is found, swap it to the beginning of tail. The first element of
--    the tail will become the closest point to the current point.
-- 3. Return the tail; iterating this algorithm on it will sort the list.
nnStep
    :: PrimMonad m
    => Distances
    -> VM.MVector (PrimState m) TspPoint
    -> m (Maybe (VM.MVector (PrimState m) TspPoint))
nnStep _ vec | VM.length vec <= 1 = pure Nothing
nnStep dm vec = do
    let (hd, tl) = VM.splitAt 1 vec
    current <- VM.read hd 0
    let loop _ i | i >= VM.length tl = pure ()
        loop minDist i = do
            candidate <- VM.read tl i
            let distanceFromCurrent = distance dm current candidate
            if distanceFromCurrent < minDist
                then VM.swap tl 0 i >> loop distanceFromCurrent (i+1)
                else loop minDist (i+1)
    loop (1/0) 0
    pure (Just tl)

-- | Abstract point in the TSP algorithms. Corresponds to the \(i\)-th input point.
newtype TspPoint = TspPoint Int
    deriving (Eq, Ord)

instance Show TspPoint where show (TspPoint p) = show p

-- | How much would the 2-opt switch change the total path length?
--
-- >>> points = [zero, Vec2 100 100, Vec2 0 100, Vec2 100 0]
-- >>> dm = distances (V.fromList points)
-- >>> tsp2optFlipDelta dm (TspPoint 0) (TspPoint 1) (TspPoint 2) (TspPoint 3)
-- -82.84271247461902
--
-- >>> points = [zero, Vec2 0 100, Vec2 100 100, Vec2 100 0]
-- >>> dm = distances (V.fromList points)
-- >>> tsp2optFlipDelta dm (TspPoint 0) (TspPoint 1) (TspPoint 2) (TspPoint 3)
-- 82.84271247461902
tsp2optFlipDelta :: Distances -> TspPoint -> TspPoint -> TspPoint -> TspPoint -> Double
tsp2optFlipDelta dm a b c d =
    let
        -- Getting these indices right cost me quite a bit of headache. Now I know
        -- I should just have written them down using pen and paper and they become
        -- quite obvious.
        --
        -- Original: a -> b -> c -> d -> a
        -- Flipped:  a -> c -> b -> d -> a
        --
        -- a <---------- d           a <---------- d
        -- |             ^            '.         .^
        -- |             |              '.     .'
        -- |             |                '. .'
        -- |   Before    |   ===>    After  X
        -- |             |                .' '.
        -- |             |              .'     '.
        -- v             |            .'         'v
        -- b ----------> c           b ----------> c
        oldLength = distance dm a b + distance dm c d
        newLength = distance dm a c + distance dm b d
    in newLength - oldLength

tsp3optFlipDelta :: Distances -> TspPoint -> TspPoint -> TspPoint -> TspPoint -> TspPoint -> TspPoint -> Vector Double
tsp3optFlipDelta dm a b c d e f =
    let dist = distance dm
        oldLength = dist a b + dist c d + dist e f
        -- I don’t think I’ll paint the ASCII art for 7 diagrams here ugh.
    in V.fromList
        [ 0                                 -- Original: { a b c d e f a }
        , dist a e + dist b f - dist a b - dist e f   -- { a e d c b f a }
        , dist c e + dist d f - dist c d - dist e f   -- { a b c e d f a }
        , dist a c + dist b d - dist a b - dist c d   -- { a c b d e f a }
        , dist a c + dist b e + dist d f - oldLength  -- { a c b e d f a }
        , dist a e + dist d b + dist c f - oldLength  -- { a e d b c f a }
        , dist a d + dist e c + dist b f - oldLength  -- { a d e c b f a }
        , dist a d + dist e b + dist c f - oldLength  -- { a d e b c f a }
        ]

-- | Inplace reverse part of a 'STVector'.
--
-- This is more complicated than the standard »swap around the middle« reversal
-- algorithm, because we wrap around the ends of the vector for j<i, which happens
-- when the swapping window starts at the end of the input. This is not a problem
-- for 2-gen, but in 3-gen we cannot simply reverse »the internal half«, because
-- there might be multiple overlapping swapping regions.
--
-- >>> input = V.fromList [0..10]
-- >>> V.modify (tspReverseInplace 3 6) input
-- [0,1,2,6,5,4,3,7,8,9,10]
--
-- V.modify (tspReverseInplace 8 3) input
-- [0,10,9,8,4,5,6,7,3,2,1]
--
-- >>> V.modify (tspReverseInplace 8 3) (V.init input)
-- [1,0,9,8,4,5,6,7,3,2]
--
-- >>> V.modify (tspReverseInplace 6 6) input
-- [0,1,2,3,4,5,6,7,8,9,10]
tspReverseInplace :: PrimMonad m => Int -> Int -> VM.MVector (PrimState m) a -> m ()
tspReverseInplace i j vec = go steps i j
  where
    n = VM.length vec
    steps = ((j-i+1) % n) `div` 2 -- Why the +1? Beats me. I spent way too much time on this tiny alg already.
    go s _ _ | s == 0 = pure ()
    go s ii jj = do
        VM.swap vec (ii%n) (jj%n)
        go (s-1) (ii+1) (jj-1)

-- | How much would the path length change if the two segments were swapped?
--
-- Does not modify the input vector, but marked as @unsafe@ because out of bounds
-- will 'error'.
--
-- For a bit more on the args’ constraints, see 'unsafeCommit3optSwap'.
unsafePlan2optSwap
    :: PrimMonad m
    => Distances -- \(n\times n\) matrix
    -> Int -- ^ \(i \in [0\ldots n-3]\)
    -> Int -- ^ \(j \in [i+2\ldots n-1]\)
    -> VM.MVector (PrimState m) TspPoint -- ^ \(n\) entries
    -> m Double -- ^ Length delta. Negative means the swap shortens the path.
unsafePlan2optSwap dm i j path = do
    let n = size dm
    a <- VM.read path i
    b <- VM.read path (i+1 % n)
    c <- VM.read path j
    d <- VM.read path (j+1 % n)
    pure (tsp2optFlipDelta dm a b c d)

-- | Perform a 2-opt swap as specified.
--
-- The requirements on \(i,j\) are not checked, hence @unsafe@. For details see
-- 'unsafeCommit3optSwap'.
unsafeCommit2optSwap
    :: PrimMonad m
    => Int -- ^ \(i \in [0\ldots n-3]\)
    -> Int -- ^ \(j \in [i+2\ldots n-1]\)
    -> VM.MVector (PrimState m) a -- ^ \(n\) entries
    -> m ()
unsafeCommit2optSwap i j path = tspReverseInplace (i+1) j path

-- | Inplace version of 'tsp2optPath'.
tsp2optInplace :: PrimMonad m => Distances -> VM.MVector (PrimState m) TspPoint -> m ()
tsp2optInplace dm path = loop0
  where
    n = size dm
    loop0 = loop 0 2
    loop i j
        | i > n-3 = pure ()          -- i exhausted, search done
        | j > n-1 = loop (i+1) (i+3) -- j exhausted, increase i
    loop i j = do
        delta <- unsafePlan2optSwap dm i j path
        if delta < 0
            then unsafeCommit2optSwap i j path >> loop0
            else loop i (j+1)

-- | Swapping cases of 3-opt. Created by 'unsafePlan3optSwap', consumed by
-- 'unsafeCommit3optSwap'.
data Case3opt
    = Case3opt1
    | Case3opt2
    | Case3opt3
    | Case3opt4
    | Case3opt5
    | Case3opt6
    | Case3opt7
    deriving (Eq, Ord, Show, Enum)

-- | If an improvement can be made by 3 segments, which case yields the largest
-- improvement, and by what amount?
--
-- Does not modify the input vector, but marked as @unsafe@ because out of bounds
-- will 'error'.
--
-- For a bit more on the args’ constraints, see 'unsafeCommit3optSwap'.
unsafePlan3optSwap
    :: PrimMonad m
    => Distances -- ^ \(n\times n\) matrix
    -> Int -- ^ \(i \in [0\ldots n-5]\)
    -> Int -- ^ \(j \in [i+2\ldots n-3]\)
    -> Int -- ^ \(k \in [j+2\ldots n-1]\)
    -> VM.MVector (PrimState m) TspPoint -- ^ \(n\) entries
    -> m (Maybe (Case3opt, Double)) -- ^ Best case, and the (negative) length delta compared to the old path.
unsafePlan3optSwap dm i j k path = do
    let n = size dm
    a <- VM.read path i
    b <- VM.read path (i+1 % n)
    c <- VM.read path j
    d <- VM.read path (j+1 % n)
    e <- VM.read path k
    f <- VM.read path (k+1 % n)

    let cases = tsp3optFlipDelta dm a b c d e f
        bestCase = V.minIndex cases
        bestDelta = cases!bestCase

    pure $ case bestCase of
        0 -> Nothing
        _ | bestDelta > -1e-8 -> Nothing -- To avoid flaky super-tiny improvements mostly introduced by numerical instabilities
        _otherwise -> Just (toEnum (bestCase-1), bestDelta)

-- | Perform a 3-opt swap as specified.
--
-- The requirements on \(i,j,k\) are not checked, hence @unsafe@. Their purpose is
-- making a minimal search of the problem space possible, exploiting all symmetries
-- available by a symmetrical distance function and the 3-opt swaps:
--
--  * We can choose \(i<j<k\) WLOG, because 3-opt swaps are 3-symmetric.
--  * \(i\) needs to leave space for \(i+1,j,j+1,k,k+1\), hence the \(n-5\).
--  * Similarly, \(j\) needs to leave space for \(j+1,k,k+1\), hence the \(n-3\).
--  * \(k\) can point to the very end, leaving \(k+1\) to cycle around to the
--    beginning.
unsafeCommit3optSwap
    :: PrimMonad m
    => Case3opt
    -> Int -- ^ \(i \in [0\ldots n-5]\)
    -> Int -- ^ \(j \in [i+2\ldots n-3]\)
    -> Int -- ^ \(k \in [j+2\ldots n-1]\)
    -> Int -- ^ \(n\)
    -> VM.MVector (PrimState m) a -- ^ \(n\) points
    -> m ()
unsafeCommit3optSwap bestCase i j k n path = case bestCase of
    Case3opt1 -> do
        tspReverseInplace (k+1 % n) i path
    Case3opt2 -> do
        tspReverseInplace (j+1    ) k path
    Case3opt3 -> do
        tspReverseInplace (i+1    ) j path
    Case3opt4 -> do
        tspReverseInplace (j+1    ) k path
        tspReverseInplace (i+1    ) j path
    Case3opt5 -> do
        tspReverseInplace (k+1 % n) i path
        tspReverseInplace (i+1    ) j path
    Case3opt6 -> do
        tspReverseInplace (k+1 % n) i path
        tspReverseInplace (j+1    ) k path
    Case3opt7 -> do
        tspReverseInplace (k+1 % n) i path
        tspReverseInplace (i+1    ) j path
        tspReverseInplace (j+1    ) k path

-- | Inplace version of 'tsp3optPath'.
tsp3optInplace :: PrimMonad m => Distances -> VM.MVector (PrimState m) TspPoint -> m ()
tsp3optInplace dm path = loop0
  where
    n = size dm
    loop0 = loop 0 2 4
    loop i j k
        | i > n-5 = pure ()                -- i exhausted, search done
        | j > n-3 = loop (i+1) (i+3) (i+5) -- j exhausted, increase i
        | k > n-1 = loop  i    (j+1) (j+3) -- k exhausted, increase j
    loop i j k = do
        result <- unsafePlan3optSwap dm i j k path
        case result of
            Nothing -> loop i j (k+1)
            Just (bestCase, _delta) -> do
                unsafeCommit3optSwap bestCase i j k n path
                loop0

-- | Move towards a local minimum using the 2-opt algorithm: delete 2 edges, and if
-- the reconnected version is shorter than the original. The result of this algorithm
-- is guaranteed to be at most as large as its input.
--
-- Starting this algoritm from a path optimized by 'tspNearestNeighbourPath' speeds
-- it up considerably, although this usually yields a different but similarly good
-- result than applying it directly.
--
-- <<docs/haddock/Numerics/Optimization/TSP/2-opt.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Numerics/Optimization/TSP/2-opt.svg" 300 200 $ do
--    points <- liftIO $ do
--        gen <- MWC.initialize (V.fromList [])
--        uniform <- uniformlyDistributedPoints gen (shrinkBoundingBox 10 [zero, Vec2 300 200]) 32
--        pure uniform
--    setLineJoin LineJoinRound
--    let dm = distances points
--        path = tsp2optPath dm (tspNearestNeighbourPath dm)
--        tour = Polygon (V.toList (pathToPoints points path))
--    cairoScope $ do
--        setColor (mathematica97 0)
--        for_ points $ \p -> sketch (Circle p 3) >> fill
--    cairoScope $ do
--        setColor (mathematica97 3)
--        sketch tour
--        stroke
-- :}
-- docs/haddock/Numerics/Optimization/TSP/2-opt.svg
tsp2optPath :: Distances -> Vector TspPoint -> Vector TspPoint
tsp2optPath dm = V.modify (tsp2optInplace dm)

-- | Move towards a local minimum using the 3-opt algorithm: delete 3 edges, and if
-- the reconnected version is shorter than the original. The result of this
-- algorithm is guaranteed to be at most as large as its input.
--
-- 3-opt yields higher quality results than 2-opt, but is considerably slower. It
-- is highly advised to only run it on already optimized paths, e.g. starting from
-- 'tspNearestNeighbourPath' or 'tsp2optPath'.
--
-- <<docs/haddock/Numerics/Optimization/TSP/3-opt.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Numerics/Optimization/TSP/3-opt.svg" 300 200 $ do
--    points <- liftIO $ do
--        gen <- MWC.initialize (V.fromList [])
--        uniform <- uniformlyDistributedPoints gen (shrinkBoundingBox 10 [zero, Vec2 300 200]) 32
--        pure uniform
--    setLineJoin LineJoinRound
--    let dm = distances points
--        path = tsp3optPath dm (tspNearestNeighbourPath dm)
--        tour = Polygon (V.toList (pathToPoints points path))
--    cairoScope $ do
--        setColor (mathematica97 0)
--        for_ points $ \p -> sketch (Circle p 3) >> fill
--    cairoScope $ do
--        setColor (mathematica97 4)
--        sketch tour
--        stroke
-- :}
-- docs/haddock/Numerics/Optimization/TSP/3-opt.svg
tsp3optPath :: Distances -> Vector TspPoint -> Vector TspPoint
tsp3optPath dm = V.modify (tsp3optInplace dm)

anneal2optStep
    :: (StatefulGen gen m, PrimMonad m)
    => gen
    -> Distances
    -> Int
    -> VM.MVector (PrimState m) TspPoint
    -> Double
    -> m ()
anneal2optStep _ _ searchGas _ _ | searchGas <= 0 = pure ()
anneal2optStep gen dm searchGas state t = do
    let n = size dm
    i <- uniformRM (0, n-1) gen
    j <- do
        -- pick j so it’s at least 2 further than i so the segments don’t overlap
        dj <- uniformRM (2, n-2) gen
        pure (i+dj % n)
    let commit = tspReverseInplace (i+1 % n) j state
        reject = anneal2optStep gen dm (searchGas-1) state t

    delta <- unsafePlan2optSwap dm i j state
    if delta < 0
        then commit
        else do
            let p = clamp (0,1) (exp (- delta / t))
            accept <- trueWithProbability gen p
            if accept
                then commit
                else reject

trueWithProbability :: (StatefulGen gen m) => gen -> Double -> m Bool
trueWithProbability gen p = do
    acceptThreshold <- uniformRM (0,1) gen
    pure (p >= acceptThreshold)

anneal2opt
    :: (StatefulGen gen m, PrimMonad m)
    => gen
    -> Distances
    -> Double
    -> Int
    -> Vector TspPoint
    -> m (Vector TspPoint)
anneal2opt gen dm t0 numSteps points = do
    state <- V.thaw points

    let
        tLin    alpha k = t0 / (1 + alpha*fromIntegral k)
        tSquare alpha k = t0 / (1 + alpha*fromIntegral k^2)
        tLog    alpha k = t0 / (1 + alpha * log (1 + fromIntegral k))
        tExp    alpha k = t0 * alpha^k -- alpha 0.8-0.9

        t = tLin 0.5

        loop k | k >= numSteps = pure ()
        loop k = do
            -- for_ [0..100] $ \_ -> do
            anneal2optStep gen dm 100 state (t k)
            loop (k+1)
    loop 0

    V.unsafeFreeze state
