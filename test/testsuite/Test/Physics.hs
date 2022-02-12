module Test.Physics where

import qualified Graphics.Rendering.Cairo as Cairo

import Draw
import Geometry
import Numerics.VectorAnalysis
import Numerics.DifferentialEquation
import Physics

import Test.TastyAll



tests :: TestTree
tests = testGroup "Physics"
    [ testParticle
    , testTwoBody
    , testThreeBody
    ]


testParticle :: TestTree
testParticle = testVisual "Particle in Gauß potential" 200 200 "docs/physics/particleInPotential" $ \_ -> do
    let potential = coulombPotential (-10) (Vec2 100 100)
        iso = isoLines (Grid (Vec2 0 0, Vec2 200 200) (20, 20)) potential

    for_ [-0.5,-0.4..0] $ \h -> for_ (iso h) $ \ps -> cairoScope $ do
        sketch ps
        setColor (viridis (-h/10000))
        Cairo.setLineWidth 1
        Cairo.stroke

    let particle = PhaseSpace { p = Vec2 0.1 0.1, q = Vec2 100 30 }
        trajectory = rungeKuttaConstantStep (const (particleInPotential 1 potential)) particle 0 0.5

    sketch (fmap (q . snd) (take 2000 trajectory))
    Cairo.stroke

testTwoBody :: TestTree
testTwoBody = testVisual "Two-Body simulation" 200 200 "docs/physics/twoBody" $ \_ -> do
    let interactionPotential = harmonicPotential (10, 10)
        externalPotential = harmonicPotential (10, 10) (Vec2 100 100)
        particle1 = PhaseSpace { p = Vec2 (-8) 2, q = Vec2 100 50 }
        particle2 = PhaseSpace { p = Vec2 10 0, q = Vec2 100 150 }
        trajectories = rungeKuttaConstantStep (const (twoBody externalPotential interactionPotential (2, 1))) (particle1, particle2) 0 0.1
        (trajectory1, trajectory2) = unzip (snd <$> trajectories)

    pathSketch (fmap q (take 1000 trajectory1))
    setColor (mathematica97 0)
    Cairo.stroke
    pathSketch (fmap q (take 1000 trajectory2))
    setColor (mathematica97 1)
    Cairo.stroke

testThreeBody :: TestTree
testThreeBody = testVisual "Three-Body simulation" 200 200 "docs/physics/threeBody" $ \_ -> do
    let interactionPotential = coulombPotential (-10000)
        particle1 = PhaseSpace { p = Vec2 (-8) 2, q = Vec2 100 50 }
        particle2 = PhaseSpace { p = Vec2 10 0, q = Vec2 100 150 }
        particle3 = PhaseSpace { p = Vec2 0 0, q = Vec2 100 100 }
        masses = NBody [1, 2, 5]
        particles = NBody [particle1, particle2, particle3]
        trajectories = traverse snd $ take 2000 $ rungeKuttaConstantStep (const (nBody zero interactionPotential masses)) particles 0 0.01

    for_ (zip [0..] (getNBody trajectories)) $ \(i, trajectory) -> do
        pathSketch (fmap q trajectory)
        setColor (mathematica97 i)
        Cairo.stroke
