{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Draw.GCode (
      renderGCode
    , addHeaderFooter
    , draw
    , ToGCode(..)
    , GCode(..)
) where



import           Data.Foldable
import           Data.Text.Lazy  (Text)
import qualified Data.Text.Lazy  as T
import           Formatting      hiding (center)
import           Geometry.Bezier
import           Geometry.Core



decimal :: Format r (Double -> r)
decimal = fixed 3

draw :: GCode -> GCode
draw content = GBlock
    [ G00_LinearRapidMove Nothing Nothing (Just (-2))
    , content
    , G00_LinearRapidMove Nothing Nothing (Just 2)
    ]

data GCode
    = GComment Text
    | GBlock [GCode]
    | F_Feedrate Double
    | M0_Pause

    | G00_LinearRapidMove (Maybe Double) (Maybe Double) (Maybe Double) -- ^ X, Y, Z
    | G01_LinearFeedrateMove (Maybe Double) (Maybe Double) (Maybe Double) -- ^ X, Y, Z
    | G02_ArcClockwise Double Double Double Double -- ^ I,J ; X,Y
    | G03_ArcCounterClockwise Double Double Double Double -- ^ I,J ; X,Y
    | G90_AbsoluteMovement
    | G91_RelativeMovement

addHeaderFooter :: GCode -> GCode
addHeaderFooter body = GBlock [header, body, footer]
  where
    header = GBlock
        [ GComment "NOOP move to make sure feedrate is set"
        , G91_RelativeMovement
        , G01_LinearFeedrateMove (Just 0) (Just 0) (Just 0)
        , G90_AbsoluteMovement
        ]

    footer = GBlock
        [ GComment "Lift pen"
        , G00_LinearRapidMove Nothing Nothing (Just 10)
        ]

renderGCode :: GCode -> Text
renderGCode = \case
    GComment comment -> "; " <> comment
    GBlock content   -> (T.unlines . filter (not . T.null) . fmap renderGCode) (GComment "{" : content <> [GComment "}"])
    F_Feedrate f     -> format ("F" % decimal) f
    M0_Pause         -> "M0"

    G00_LinearRapidMove Nothing Nothing Nothing -> mempty
    G00_LinearRapidMove x y z                   -> format ("G0" % optioned (" X"%decimal) % optioned (" Y"%decimal) % optioned (" Z"%decimal)) x y z

    G01_LinearFeedrateMove Nothing Nothing Nothing -> mempty
    G01_LinearFeedrateMove x y z                   -> format ("G1" % optioned (" X"%decimal) % optioned (" Y"%decimal) % optioned (" Z"%decimal)) x y z

    G02_ArcClockwise        i j x y -> format ("G2 X" % decimal % " Y" % decimal % " I" % decimal % " J" % decimal) x y i j
    G03_ArcCounterClockwise i j x y -> format ("G3 X" % decimal % " Y" % decimal % " I" % decimal % " J" % decimal) x y i j

    G90_AbsoluteMovement -> "G90"
    G91_RelativeMovement -> "G91"

class ToGCode a where
    toGCode :: a -> GCode

instance ToGCode GCode where
    toGCode = id

-- | Trace the bounding box without actually drawing anything to estimate result size
instance ToGCode BoundingBox where
    toGCode (BoundingBox (Vec2 xMin yMin) (Vec2 xMax yMax)) = GBlock
        [ GComment "Hover over bounding box"
        , G00_LinearRapidMove (Just xMin) (Just yMin) Nothing
        , G00_LinearRapidMove (Just xMax) (Just yMin) Nothing
        , G00_LinearRapidMove (Just xMax) (Just yMax) Nothing
        , G00_LinearRapidMove (Just xMin) (Just yMax) Nothing
        , G00_LinearRapidMove (Just xMin) (Just yMin) Nothing
        ]

instance ToGCode Circle where
    toGCode (Circle (Vec2 x y) r) =
        let (startX, startY) = (x-r, y)
        in GBlock
            [ GComment "Circle"
            , G00_LinearRapidMove (Just startX) (Just startY) Nothing
            , draw (G02_ArcClockwise r 0 startX startY)
            ]

-- | Polyline
instance {-# OVERLAPPING #-} Sequential f => ToGCode (f Vec2) where
    toGCode = go . toList
      where
        go [] = GBlock []
        go (Vec2 startX startY : points) = GBlock
            [ GComment "Polyline"
            , G00_LinearRapidMove (Just startX) (Just startY) Nothing
            , draw (GBlock [ G01_LinearFeedrateMove (Just x) (Just y) Nothing | Vec2 x y <- points])
            ]

-- | Draw each element separately. Note the overlap with the Polyline instance, which takes precedence.
instance {-# OVERLAPPABLE #-} (Functor f, Sequential f, ToGCode a) => ToGCode (f a) where
    toGCode x = GBlock (GComment "Sequential" : toList (fmap toGCode x))

-- | Draw each element (in order)
instance {-# OVERLAPPING #-} (ToGCode a, ToGCode b) => ToGCode (a,b) where
    toGCode (a,b) = GBlock [GComment "2-tuple", toGCode a, toGCode b]

-- | Draw each element (in order)
instance {-# OVERLAPPING #-} (ToGCode a, ToGCode b, ToGCode c) => ToGCode (a,b,c) where
    toGCode (a,b,c) = GBlock [GComment "3-tuple", toGCode a, toGCode b, toGCode c]

-- | Draw each element (in order)
instance {-# OVERLAPPING #-} (ToGCode a, ToGCode b, ToGCode c, ToGCode d) => ToGCode (a,b,c,d) where
    toGCode (a,b,c,d) = GBlock [GComment "4-tuple", toGCode a, toGCode b, toGCode c, toGCode d]

-- | Draw each element (in order)
instance {-# OVERLAPPING #-} (ToGCode a, ToGCode b, ToGCode c, ToGCode d, ToGCode e) => ToGCode (a,b,c,d,e) where
    toGCode (a,b,c,d,e) = GBlock [GComment "5-tuple", toGCode a, toGCode b, toGCode c, toGCode d, toGCode e]

instance ToGCode Polygon where
    toGCode (Polygon []) = GBlock []
    toGCode (Polygon (p:ps)) = GBlock -- Like polyline, but closes up the shape
        [ GComment "Polygon"
        , let Vec2 startX startY = p in G00_LinearRapidMove (Just startX) (Just startY) Nothing
        , draw (GBlock [G01_LinearFeedrateMove (Just x) (Just y) Nothing | Vec2 x y <- ps ++ [p]])
        ]

-- | FluidNC doesn’t support G05, so we approximate Bezier curves with line pieces.
-- We use the naive Bezier interpolation 'bezierSubdivideT', because it just so
-- happens to put more points in places with more curvature.
instance ToGCode Bezier where
    toGCode bezier@(Bezier a _ _ _) = GBlock
        [ GComment "Bezier (cubic)"
        , let Vec2 startX startY = a in G00_LinearRapidMove (Just startX) (Just startY) Nothing
        , draw (GBlock [G01_LinearFeedrateMove (Just x) (Just y) Nothing | Vec2 x y <- bezierSubdivideT 32 bezier])
        ]
