{-# LANGUAGE DeriveFunctor #-}
module Main where



import Control.Monad.ST
import qualified Graphics.Rendering.Cairo as C
import System.Random.MWC
import qualified Util.RTree as RT

import Draw
import Geometry
import Geometry.Chaotic



data Config = Config
    { branchAngle :: Angle
    , branchProbability :: Double
    , branchGrowth :: Double
    , minLength :: Double
    }

mainBranchConfig, sideBranchConfig :: Config
mainBranchConfig = Config
    { branchAngle = deg 2.5
    , branchProbability = 1
    , branchGrowth = 0.99
    , minLength = 0.1
    }
sideBranchConfig = Config
    { branchAngle = deg 2.5
    , branchProbability = 0.05
    , branchGrowth = 0.9
    , minLength = 0.5
    }

picWidth, picHeight :: Num a => a
picWidth = 500
picHeight = 500

main :: IO ()
main = do
    let initialBranch = Branch (Vec2 (picWidth/2) (picHeight-1)) 10 (deg 0)
        picEdges =
            [ Line (Vec2 0 0) (Vec2 0 picHeight)
            , Line (Vec2 0 picHeight) (Vec2 picWidth picHeight)
            , Line (Vec2 picWidth picHeight) (Vec2 picWidth 0)
            , Line (Vec2 picWidth 0) (Vec2 0 0)
            ]
        tree = fmap fst $ runST $ do
            gen <- initializeMwc (2 :: Double)
            grow gen CW (RT.fromList picEdges) initialBranch
    render "out/tendrils.png" picWidth picHeight $ do
        coordinateSystem (MathStandard_ZeroBottomLeft_XRight_YUp picHeight)
        cairoScope (setColor white >> C.paint)
        drawTree tree

drawTree :: Tree Branch -> C.Render ()
drawTree (Node (Branch p _ _) a b) = do
    drawBranchMaybe (Just a)
    drawBranchMaybe b
  where
    drawBranchMaybe = \case
        Just tree@(Node branch _ _) -> drawBranch p branch >> drawTree tree
        _otherwise -> pure ()
drawTree _ = pure ()

drawBranch :: Vec2 -> Branch -> C.Render ()
drawBranch p (Branch q _ _) = do
    sketch (Line p q)
    C.stroke

data Tree a
    = Node a (Tree a) (Maybe (Tree a))
    | Tip
    | Collision
    deriving (Functor)

grow :: GenST s -> Curvature -> RT.RTree Line -> Branch -> ST s (Tree (Branch, RT.RTree Line))
grow gen curv lines branch = step gen mainBranchConfig curv lines branch >>= \case
    Collision -> pure Collision
    Tip -> pure (Node (branch, lines) Tip Nothing)
    mb@(Node (_, ls) _ _) -> step gen sideBranchConfig (other curv) ls branch >>= \case
        sb@(Node (_, ls') _ _) -> pure (Node (branch, ls') mb (Just sb))
        _otherwise       -> pure (Node (branch, ls) mb Nothing)

data Curvature = CW | CCW

other :: Curvature -> Curvature
other CW = CCW
other CCW = CW

curvSign :: Num a => Curvature -> a
curvSign CCW = 1
curvSign CW = -1

data Branch = Branch Vec2 Double Angle

step :: GenST s -> Config -> Curvature -> RT.RTree Line -> Branch -> ST s (Tree (Branch, RT.RTree Line))
step gen config curv lines (Branch p l a) = dice gen (branchProbability config) >>= \case
    False -> pure Tip
    True | collides -> pure Collision
         | l <= minLength config -> pure Tip
         | otherwise -> do
            let lines' = RT.insert line lines
                branch' = Branch p' l' a'
            grow gen curv lines' branch'
  where
    l' = l * branchGrowth config
    a' = a +. (curvSign curv *. branchAngle config)
    line@(Line _ p') = angledLine p a' l'
    collides = any (realIntersection line) (RT.intersect (boundingBox line) lines)
    realIntersection l1@(Line p1 q1) l2@(Line p2 q2) = case intersectionLL l1 l2 of
        IntersectionReal _ -> p1 /= p2 && p1 /= q2 && q1 /= p2 && q1 /= q2
        _                  -> False

dice :: GenST s -> Double -> ST s Bool
dice gen threshold = do
    throw <- uniformRM (0, 1) gen
    pure (throw <= threshold)

filterMaybe :: Bool -> Maybe a -> Maybe a
filterMaybe True = id
filterMaybe False = const Nothing

unzipMaybe :: Maybe (a, b) -> (Maybe a, Maybe b)
unzipMaybe (Just (a, b)) = (Just a, Just b)
unzipMaybe Nothing = (Nothing, Nothing)
