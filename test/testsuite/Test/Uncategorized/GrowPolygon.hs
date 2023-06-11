module Test.Uncategorized.GrowPolygon (tests) where



import Graphics.Rendering.Cairo as C hiding (x, y)

import Draw
import Geometry

import Test.TastyAll



tests :: TestTree
tests = testGroup "Polygon growing/shrinking"
    [ shrinkWithEar
    ]

shrinkWithEar :: TestTree
shrinkWithEar = testVisual "Shrink polygon, producing an ear (that should be clipped)" 100 100 "docs/geometry/grow_polygon_shrink_ear" $ \_ -> do
    let polygon = Polygon [Vec2 10 10, Vec2 85 10, Vec2 90 15, Vec2 90 90, Vec2 10 90]
    cairoScope $ do
        setColor (mma 0)
        for_ (zip [0..] (polygonEdges polygon)) $ \(i, edge) -> do
            setColor (mma i)
            sketch edge
            stroke
    cairoScope $ do
        let polygon' = growPolygon (-20) polygon
        for_ (zip [0..] (polygonEdges polygon')) $ \(i, edge) -> do
            setColor (mma i)
            sketch edge
            stroke
        stroke
