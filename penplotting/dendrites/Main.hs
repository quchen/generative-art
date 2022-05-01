module Main (main) where



import Control.Monad
import qualified Data.Heap as H
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Graphics.Rendering.Cairo as C
import System.Random.MWC
import System.Random.MWC.Distributions

import Draw
import Geometry



picWidth, picHeight :: Num a => a
picWidth = 1000
picHeight = 1000

canvas :: BoundingBox
canvas = boundingBox [zero, Vec2 picWidth picHeight]

main :: IO ()
main = do
    gen <- create
    let seeds = [Vec2 100 100, Vec2 500 100, Vec2 900 100]
        radius = 10
    dendrites <- growDendrites gen seeds radius

    render "out/dendrites.png" picWidth picHeight $ do
        coordinateSystem (MathStandard_ZeroBottomLeft_XRight_YUp picHeight)
        cairoScope (setColor white >> C.paint)
        for_ (zip [0..] dendrites) $ \(c, d) -> do
            setColor (mathematica97 c)
            drawDendrite (root d) d


drawDendrite :: Vec2 -> Dendrite -> C.Render ()
drawDendrite parent (Node p children) = do
    sketch (Line parent p)
    C.stroke
    when (null children) $ do
        sketch (Circle p 2)
        C.stroke
    for_ children $ drawDendrite p

growDendrites :: GenIO -> [Vec2] -> Double -> IO [Dendrite]
growDendrites gen seeds radius = fmap _result <$> loop (S.fromList seeds) (V.fromList (initialState <$> seeds))
  where
    initialState p = GrowthState
        { _result = seed p
        , _activeBranches = H.singleton (H.Entry 0 p)
        , _allNodes = S.singleton p
        , _radius = radius
        }
    loop allNodes states = do
        let (inactive, active) = V.partition (H.null . _activeBranches) states
        uniformShuffle active gen >>= \shuffled -> case V.uncons shuffled of
            Nothing -> pure (V.toList states)
            Just (item, otherActive) -> do
                (allNodes', item') <- growCell allNodes item
                loop allNodes' (V.concat [inactive, V.singleton item', otherActive])

    growCell allNodes state = do
        state' <- grow gen state { _allNodes = allNodes }
        pure (allNodes `S.union` _allNodes state', state')

data Dendrite = Node Vec2 [Dendrite]

seed :: Vec2 -> Dendrite
seed p = Node p []

root :: Dendrite -> Vec2
root (Node p _) = p

insertAt :: Vec2 -> Vec2 -> Dendrite -> Dendrite
insertAt target newItem (Node p nodes)
    | p == target
    = Node p (seed newItem : nodes)
    | otherwise
    = Node p (insertAt target newItem <$> nodes)

data GrowthState = GrowthState
    { _result :: Dendrite
    , _activeBranches :: H.Heap (H.Entry Double Vec2)
    , _allNodes :: S.Set Vec2
    , _radius :: Double
    }

grow :: GenIO -> GrowthState -> IO GrowthState
grow gen s@GrowthState{..} = case H.uncons _activeBranches of
    Nothing -> pure s
    Just (H.Entry _ closestActiveBranch, activeBranches') -> candidates gen s closestActiveBranch >>= \case
        [] -> pure s { _activeBranches = activeBranches' }
        p : _ -> pure s
            { _result = insertAt closestActiveBranch p _result
            , _allNodes = S.insert p _allNodes
            , _activeBranches = H.insert (H.Entry (norm (p -. root _result)) p) _activeBranches
            }

candidates :: GenIO -> GrowthState -> Vec2 -> IO [Vec2]
candidates gen GrowthState{..} p = fmap catMaybes $ replicateM 10 $ do
    phi <- rad <$> uniformRM (0, 2*pi) gen
    r' <- uniformRM (_radius, 2*_radius) gen
    let p' = p +. polar phi r'
    pure $ do
        guard (p' `insideBoundingBox` canvas)
        guard (not (any (\q -> norm (p' -. q) <= _radius) _allNodes))
        Just p'


