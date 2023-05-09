-- | Nice API for Delaunator’s technical output.
module Geometry.Algorithms.Delaunay.DelaunatorApi (
    delaunayTriangulation
    , Triangulation(..)
    , D.TriangulationRaw
) where



import           Control.DeepSeq
import           Data.Foldable
import           Control.Applicative
import           Control.Monad
import           Data.Vector                             (Vector, (!))
import qualified Data.Vector                             as V
import qualified Data.Vector.Mutable                             as VM
import qualified Geometry.Algorithms.Delaunay.Delaunator as D
import           Geometry.Core


data Triangulation = Triangulation
    { _points :: Vector Vec2
    -- ^ Input points to have everything in one package

    , _triangles :: Vector Polygon
    -- ^ All Delaunay triangles. Note that plotting these will have one line going
    -- back and one going forth between points not on the convex hull of the input.
    -- Use '_edges' if this is undesirable.

    , _edges :: Vector Line
    -- ^ Each (undirected) edge of the Delaunay triangulation.

    , _raw :: D.TriangulationRaw
    -- ^ Raw triangulation data. Import the internal module to access its constructors.

    } deriving (Eq, Ord, Show)

delaunayTriangulation :: Sequential vector => vector Vec2 -> Triangulation
delaunayTriangulation points' =
    let points = toVector points'
        raw = D.triangulate points
    in Triangulation
        { _points = points
        , _triangles = triangles points raw
        , _edges = edges points raw
        , _raw = raw
        }

instance NFData Triangulation where
    rnf (Triangulation a b c d) = rnf (a,b,c,d)

triangles :: Vector Vec2 -> D.TriangulationRaw -> Vector Polygon
triangles points triangulation =
    let triangleIxs = D._triangles triangulation
        numTriangles = V.length triangleIxs `div` 3
        corners = V.backpermute points triangleIxs
        -- I have a hunch this could be done with another single call to backpermute instead of generate/iterate…
    in V.generate numTriangles (pickPolygon corners)

pickPolygon :: Vector Vec2 -> Int -> Polygon
pickPolygon corners i = Polygon (map (corners!) [3*i, 3*i+1, 3*i+2])

edges :: Vector Vec2 -> D.TriangulationRaw -> Vector Line
edges points triangulation = V.fromList $ do
    let triangleIxs = D._triangles triangulation
        halfedgeIxs = D._halfedges triangulation
        numHalfedges = V.length halfedgeIxs
    e <- [0..numHalfedges - 1]

    -- We arbitrarily select the larger of the two edges here. Note that this also
    -- covers the pair-less case, in which the opposite halfedge has index -1.
    guard (e > halfedgeIxs!e)
    let p = points!(triangleIxs!e)
        q = points!(triangleIxs!D.nextHalfedge e)
    pure (Line p q)
