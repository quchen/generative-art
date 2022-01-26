module Geometry.Processes.Billard (
    billard
) where



import Algebra.VectorSpace
import Control.Monad
import Data.List
import Data.Maybe

import Geometry.Core



-- | Shoot a billard ball, and record its trajectory as it is reflected off the
-- edges of a provided geometry.
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
