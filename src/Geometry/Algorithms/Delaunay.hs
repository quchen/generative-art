module Geometry.Algorithms.Delaunay (
  DelaunayTriangulation()
, getPolygons
, bowyerWatson
, bowyerWatsonStep

, toVoronoi
, lloydRelaxation
) where

import Geometry.Algorithms.Delaunay.Internal
