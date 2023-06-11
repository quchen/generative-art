module Geometry.Processes.Billard (
    billard
) where



import Algebra.VectorSpace
import Control.Monad
import Data.List
import Data.Maybe

import Geometry.Core




-- $setup
-- >>> import Draw
-- >>> import qualified Graphics.Rendering.Cairo as C


-- | Shoot a billard ball, and record its trajectory as it is reflected off the
-- edges of a provided geometry.
--
-- <<docs/haddock/Geometry/Processes/Billard/billard.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Processes/Billard/billard.svg" 330 360 $ \_ -> do
--     let lambda = transform (translate (Vec2 10 350) <> mirrorYCoords) . Polygon $
--             [ Vec2 0.387   340.156
--             , Vec2 113.773 170.078
--             , Vec2 0.387   0
--             , Vec2 85.426  0
--             , Vec2 312.195 340.156
--             , Vec2 227.156 340.156
--             , Vec2 156.293 233.859
--             , Vec2 85.426  340.156
--             , Vec2 0.387   340.156 ]
--         startPoint = Vec2 100 100
--         startAngle = deg (-25)
--         numReflections = 128
--         startVec = angledLine startPoint startAngle 100
--         billardPoints = startPoint : take numReflections (billard (polygonEdges lambda) startVec)
--     cairoScope $ do
--         setColor (mma 0)
--         C.setDash [2,4] 0
--         sketch lambda
--         C.stroke
--     cairoScope $ do
--         setColor (mma 0)
--         for_ billardPoints $ \point -> sketch (Circle point 3) >> C.stroke
--     cairoScope $ do
--         setColor (mma 1)
--         let billardArrows = zipWith Line billardPoints (tail billardPoints)
--         for_ billardArrows $ \arr -> sketch arr >> C.stroke
-- :}
-- Generated file: size 80KB, crc32: 0xd6e508bf
billard
    :: [Line] -- ^ Geometry; typically involves the edges of a bounding polygon.
    -> Line   -- ^ Initial velocity vector of the ball. Only start and direction,
              --   not length, are relevant for the algorithm.
    -> [Vec2] -- ^ List of collision points. Finite iff the ball escapes the
              --   geometry.
billard edges = go (const True)
  where
    -- The predicate is used to exclude the line just mirrored off of, otherwise
    -- we get rays stuck in a single line due to numerical shenanigans. Note
    -- that this is a valid use case for equality of Double (contained in
    -- Line/Vec2). :-)
    go :: (Line -> Bool) -> Line -> [Vec2]
    go considerEdge ballVec@(Line ballStart _)
      = let reflectionRays :: [(Line, Line)]
            reflectionRays = do
                edge <- edges
                (Line _ reflectionEnd, incidentPoint, ty) <- maybeToList (reflection ballVec edge)
                guard $ case ty of
                    IntersectionReal _           -> True
                    IntersectionVirtualInsideR _ -> True
                    _otherwise                   -> False
                guard (incidentPoint `liesAheadOf` ballVec)
                guard (considerEdge edge)
                pure (edge, Line incidentPoint reflectionEnd)

        in case reflectionRays of
            [] -> let Line _ end = ballVec in [end]
            _  ->
                let (edgeReflectedOn, reflectionRay@(Line reflectionStart _))
                      = minimumBy
                          (\(_, Line p _) (_, Line q _) -> distanceFrom ballStart p q)
                          reflectionRays
                in reflectionStart : go (/= edgeReflectedOn) reflectionRay

    liesAheadOf :: Vec2 -> Line -> Bool
    liesAheadOf point (Line rayStart rayEnd)
      = dotProduct (point -. rayStart) (rayEnd -. rayStart) > 0

    distanceFrom :: Vec2 -> Vec2 -> Vec2 -> Ordering
    distanceFrom start p q
      = let pDistance = lineLength (Line start p)
            qDistance = lineLength (Line start q)
        in compare pDistance qDistance
