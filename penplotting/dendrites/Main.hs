module Main (main) where



import Control.Monad
import qualified Data.Heap as H
import Data.Maybe
import qualified Data.Set as S
import System.Random.MWC

import Draw
import Geometry



canvas :: BoundingBox
canvas = undefined

main :: IO ()
main = pure ()

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
candidates gen s@GrowthState{..} p = fmap catMaybes $ replicateM 10 $ do
    phi <- rad <$> uniformRM (0, 2*pi) gen
    r' <- uniformRM (_radius, 2*_radius) gen
    let p' = p +. polar phi r'
    pure $ do
        guard (p' `insideBoundingBox` canvas)
        guard (not (any (\q -> norm (p' -. q) <= _radius) _allNodes))
        Just p'


