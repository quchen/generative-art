{-# LANGUAGE DeriveFunctor #-}
module Main where



import Control.Monad.ST
import Control.Applicative
import qualified Graphics.Rendering.Cairo as C
import System.Random.MWC

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
    , branchProbability = 0.97
    , branchGrowth = 0.98
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
        tree = runST $ do
            gen <- initializeMwc (2 :: Double)
            grow gen CW initialBranch
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

grow :: GenST s -> Curvature -> Branch -> ST s (Tree Branch)
grow gen curv branch = do
    mainBranch <- step gen mainBranchConfig curv branch
    sideBranch <- step gen sideBranchConfig curv branch
    liftA2 (Node branch)
        (traverse (grow gen curv) mainBranch)
        (traverse (grow gen (other curv)) sideBranch)

dice :: GenST s -> Double -> ST s Bool
dice gen threshold = do
    throw <- uniformRM (0, 1) gen
    pure (throw <= threshold)

step :: GenST s -> Config -> Curvature -> Branch -> ST s (Maybe Branch)
step gen config curv (Branch p l a) = dice gen (branchProbability config) >>= \case
    False -> pure Nothing
    True  -> pure $ if l >= minLength config
        then Just (Branch p' l' a')
        else Nothing
  where
    l' = l * branchGrowth config
    a' = a +. (curvSign curv *. branchAngle config)
    Line _ p' = angledLine p a' l'

filterMaybe :: Bool -> Maybe a -> Maybe a
filterMaybe True = id
filterMaybe False = const Nothing
