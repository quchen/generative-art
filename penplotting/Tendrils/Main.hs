{-# LANGUAGE DeriveFunctor #-}
module Main where



import Control.Monad.ST
import Data.Maybe
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
    { branchAngle = deg 2
    , branchProbability = 1
    , branchGrowth = 0.96
    , minLength = 0.1
    }
sideBranchConfig = Config
    { branchAngle = deg (-10)
    , branchProbability = 0.2
    , branchGrowth = 0.5
    , minLength = 0.1
    }

picWidth, picHeight :: Num a => a
picWidth = 100
picHeight = 100

main :: IO ()
main = do
    let initialBranch = Branch (Vec2 0 (picHeight/2)) 5 (deg 0)
        (tree, _) = runST $ do
            gen <- initializeMwc (2 :: Double)
            grow gen CW (initialBranch, RT.empty)
    render "out/tendrils.png" picWidth picHeight $ do
        coordinateSystem (MathStandard_ZeroBottomLeft_XRight_YUp picHeight)
        cairoScope (setColor white >> C.paint)
        drawTree tree

drawTree :: Tree Branch -> C.Render ()
drawTree (Node (Branch p _ _) a b) = do
    drawBranchMaybe a
    drawBranchMaybe b
  where
    drawBranchMaybe = \case
        Nothing -> pure ()
        Just tree@(Node branch _ _) -> drawBranch p branch >> drawTree tree

drawBranch :: Vec2 -> Branch -> C.Render ()
drawBranch p (Branch q _ _) = do
    sketch (Line p q)
    C.stroke

data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
    deriving (Functor)

data Curvature = CW | CCW

other :: Curvature -> Curvature
other CW = CCW
other CCW = CW

curvSign :: Num a => Curvature -> a
curvSign CCW = 1
curvSign CW = -1

data Branch = Branch Vec2 Double Angle

grow :: GenST s -> Curvature -> (Branch, RT.RTree Line) -> ST s (Tree Branch, RT.RTree Line)
grow gen curv (branch, lines) = do
    (mainBranch, lines') <- fmap unzipMaybe $ step gen mainBranchConfig curv lines branch >>= traverse (grow gen curv)
    let lines'' = fromMaybe lines lines'
    (sideBranch, lines''') <- fmap unzipMaybe $ step gen sideBranchConfig curv lines'' branch >>= traverse (grow gen (other curv))
    pure (Node branch mainBranch sideBranch, fromMaybe lines'' lines''')

dice :: GenST s -> Double -> ST s Bool
dice gen threshold = do
    throw <- uniformRM (0, 1) gen
    pure (throw <= threshold)

step :: GenST s -> Config -> Curvature -> RT.RTree Line -> Branch -> ST s (Maybe (Branch, RT.RTree Line))
step gen config curv lines (Branch p l a) = dice gen (branchProbability config) >>= \case
    False -> pure Nothing
    True  -> pure $ if l >= minLength config && not collides
        then Just (Branch p' l' a', RT.insert line lines)
        else Nothing
  where
    l' = l * branchGrowth config
    a' = a +. (curvSign curv *. branchAngle config)
    line@(Line _ p') = angledLine p a' l'
    collides = any (realIntersection line) (RT.intersect (boundingBox line) lines)
    realIntersection l1@(Line p1 q1) l2@(Line p2 q2) = case intersectionLL l1 l2 of
        IntersectionReal _ -> p1 /= p2 && p1 /= q2 && q1 /= p2 && q1 /= q2
        _                  -> False

filterMaybe :: Bool -> Maybe a -> Maybe a
filterMaybe True = id
filterMaybe False = const Nothing

unzipMaybe :: Maybe (a, b) -> (Maybe a, Maybe b)
unzipMaybe (Just (a, b)) = (Just a, Just b)
unzipMaybe Nothing = (Nothing, Nothing)
