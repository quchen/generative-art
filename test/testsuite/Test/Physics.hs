module Test.Physics where

import Control.Monad.ST
import qualified Graphics.Rendering.Cairo as Cairo
import System.Random.MWC
import System.Random.MWC.Distributions

import Draw
import Geometry
import Geometry.Algorithms.Sampling
import Numerics.DifferentialEquation
import Physics

import Test.TastyAll



tests :: TestTree
tests = testGroup "Physics"
    [ testParticle
    , testTwoBody
    , testThreeBody
    , testCollision
    , testBrownianMotion
    ]


testParticle :: TestTree
testParticle = testVisual "Particle in Gauß potential" 200 200 "docs/physics/particleInPotential" $ \_ -> do
    let potential = coulombPotential (-10) (Vec2 100 100)
        iso = isoLines (Grid (Vec2 0 0, Vec2 200 200) (20, 20)) potential

    for_ [-0.5,-0.4..0] $ \h -> for_ (iso h) $ \ps -> cairoScope $ do
        sketch (Polyline ps)
        setColor (viridis (-h/10000))
        Cairo.setLineWidth 1
        Cairo.stroke

    let particle = PhaseSpace { p = Vec2 0.1 0.1, q = Vec2 100 30 }
        trajectory = rungeKuttaConstantStep (const (particleInPotential 1 potential)) particle 0 0.5

    sketch (Polyline (fmap (q . snd) (take 2000 trajectory)))
    Cairo.stroke

testTwoBody :: TestTree
testTwoBody = testVisual "Two-Body simulation" 200 200 "docs/physics/twoBody" $ \_ -> do
    let interactionPotential = harmonicPotential (10, 10)
        externalPotential = harmonicPotential (10, 10) (Vec2 100 100)
        particle1 = PhaseSpace { p = Vec2 (-8) 2, q = Vec2 100 50 }
        particle2 = PhaseSpace { p = Vec2 10 0, q = Vec2 100 150 }
        trajectories = rungeKuttaConstantStep (const (twoBody externalPotential interactionPotential (2, 1))) (particle1, particle2) 0 0.1
        (trajectory1, trajectory2) = unzip (snd <$> trajectories)

    sketch (Polyline (fmap q (take 1000 trajectory1)))
    setColor (mathematica97 0)
    Cairo.stroke
    sketch (Polyline (fmap q (take 1000 trajectory2)))
    setColor (mathematica97 1)
    Cairo.stroke

testThreeBody :: TestTree
testThreeBody = testVisual "Three-Body simulation" 200 200 "docs/physics/threeBody" $ \_ -> do
    let particle1 = PhaseSpace { p = Vec2 (-8) 2, q = Vec2 100 50 }
        particle2 = PhaseSpace { p = Vec2 10 0, q = Vec2 100 150 }
        particle3 = PhaseSpace { p = Vec2 0 0, q = Vec2 100 100 }
        masses = NBody [1, 2, 5]
        particles = NBody [particle1, particle2, particle3]
        trajectories = traverse snd $ takeWhile ((<20) . fst) $ nBodyCoulomb masses particles

    for_ (zip [0..] (getNBody trajectories)) $ \(i, trajectory) -> do
        sketch (Polyline (fmap q trajectory))
        setColor (mathematica97 i)
        Cairo.stroke

testCollision :: TestTree
testCollision = testVisual "Particle collision" 200 200 "docs/physics/collision" $ \_ -> do
    let particle1 = PhaseSpace { p = Vec2 10 0, q = Vec2 0 105 }
        particle2 = PhaseSpace { p = Vec2 (-10) 0, q = Vec2 200 100 }
        masses = NBody [1, 4]
        particles = NBody [particle1, particle2]
        trajectories = traverse snd $ takeWhile ((<20) . fst) $ nBodyCoulomb masses particles

    for_ (zip [0..] (getNBody trajectories)) $ \(i, trajectory) -> do
        sketch (Polyline (fmap q trajectory))
        setColor (mathematica97 i)
        Cairo.stroke

nBodyCoulomb :: NBody Double -> NBody PhaseSpace -> [(Double, NBody PhaseSpace)]
nBodyCoulomb masses particles =
    let interactionPotential = coulombPotential (-10000)
        toleranceNorm (NBody xs) = maximum (fmap (\PhaseSpace {..} -> max (norm p) (norm q)) xs)
        tolerance = 0.001
        initialStep = 1
        t0 = 0
    in  rungeKuttaAdaptiveStep (const (nBody zero interactionPotential masses)) particles t0 initialStep toleranceNorm tolerance

testBrownianMotion :: TestTree
testBrownianMotion = testVisual "Brownian motion" 200 200 "docs/physics/brownian" $ \_ -> do
    let particles = runST $ do
            gen <- create
            qs <- poissonDisc gen def
                { _poissonShape = BoundingBox zero (Vec2 200 200)
                , _poissonRadius = 20
                , _poissonK = 4
                }
            ps <- replicateM (length qs) $ gaussianVec2 zero 4 gen
            pure (NBody $ zipWith PhaseSpace ps qs)
        masses = pure 1
        interactionPotential = coulombPotential 100
        toleranceNorm (NBody xs) = maximum (fmap (\PhaseSpace {..} -> max (norm p) (norm q)) xs)
        tolerance = 0.1
        initialStep = 1
        t0 = 0
        trajectories = traverse snd $ takeWhile ((<20) . fst) $
            rungeKuttaAdaptiveStep (const (nBody zero interactionPotential masses)) particles t0 initialStep toleranceNorm tolerance

    for_ (zip [0..] (getNBody trajectories)) $ \(i, trajectory) -> do
        sketch (Polyline (fmap q trajectory))
        setColor (mathematica97 i)
        Cairo.stroke

gaussianVec2
    :: Vec2 -- ^ Mean
    -> Double -- ^ Standard deviation
    -> GenST s
    -> ST s Vec2
gaussianVec2 (Vec2 muX muY) sigma gen = Vec2 <$> normal muX sigma gen <*> normal muY sigma gen
