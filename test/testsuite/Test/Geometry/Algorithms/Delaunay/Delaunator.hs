module Test.Geometry.Algorithms.Delaunay.Delaunator (tests) where



import Test.TastyAll

import qualified Test.Geometry.Algorithms.Delaunay.Internal.Delaunator.Api as Api
import qualified Test.Geometry.Algorithms.Delaunay.Internal.Delaunator.Raw as Raw



tests :: TestTree
tests = testGroup "Delaunator"
    [ Raw.tests
    , Api.tests
    ]
