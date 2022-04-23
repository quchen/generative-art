module Draw.Text where



import Data.Default.Class
import qualified Graphics.PlotFont as PF
import qualified Graphics.Rendering.Cairo as C

import Geometry



-- | Vertical alignment
data VAlign = VTop | VCenter | VBottom deriving (Eq, Ord, Show)

-- | Horizontal alignment
data HAlign = HLeft | HCenter | HRight deriving (Eq, Ord, Show)

-- | Like Cairo’s 'showText', but with alignment parameters. Since Cairo’s text API
-- is pretty wonky, you may have to sprinkle this with 'moveTo'/'moveToVec' or
-- 'newPath'.
showTextAligned
    :: C.CairoString string
    => HAlign -- ^ Horizontal alignment
    -> VAlign -- ^ Vertical alignment
    -> string -- ^ Text
    -> C.Render ()
showTextAligned hAlign vAlign str = do
    (w,h) <- do ex <- C.textExtents str
                pure (C.textExtentsWidth ex, C.textExtentsHeight ex)
    let dx = case hAlign of
            HLeft   -> 0
            HCenter -> -w/2
            HRight  -> -w
        dy = case vAlign of
            VTop    -> h
            VCenter -> h/2
            VBottom -> 0
    C.relMoveTo dx dy
    C.showText str
    C.newPath -- The text API is wonky, it kinda-sorta moves the pointer but not really.
              -- newPath clears the path, so we get no leaks from the text.



data PlotTextOptions = PlotTextOptions
    { _textStartingPoint :: Vec2
        -- ^ Starting point
    , _textHeight :: Double
        -- ^ X height (i.e. target height of the letter "X")
    , _textHAlign :: HAlign
        -- Horizontal alignment of text relative to the starting point
    , _textVAlign :: VAlign
        -- Vertical alignment of the text relative to the X height
        -- (i.e. not including undercut letters like "g")
    }

instance Default PlotTextOptions where
    def = PlotTextOptions
        { _textStartingPoint = zero
        , _textHeight = 12
        , _textHAlign = HLeft
        , _textVAlign = VBottom
        }

plotText :: PlotTextOptions -> String -> [Polyline []]
plotText options text = transform (translate (_textStartingPoint options) <> scaleToHeight <> halign <> valign) glyphs
  where
    glyphs = pfPolyline <$> PF.render' PF.canvastextFont text
    BoundingBox (Vec2 xMin _) (Vec2 xMax _) = boundingBox glyphs
    halign = case _textHAlign options of
        HLeft -> mempty
        HRight -> translate (Vec2 (-xMax) 0)
        HCenter -> translate (Vec2 (- (xMin + xMax) / 2) 0)
    valign = case _textVAlign options of
        VBottom -> mempty
        VTop -> translate (Vec2 0 (-pfXHeight))
        VCenter -> translate (Vec2 0 (- pfXHeight / 2))
    scaleToHeight = scale (_textHeight options / pfXHeight)

pfXHeight :: Double
pfXHeight = yMax - yMin
  where
    BoundingBox (Vec2 _ yMin) (Vec2 _ yMax) = boundingBox (pfPolyline <$> PF.render' PF.canvastextFont "X")


pfPolyline :: PF.PFStroke -> Polyline []
pfPolyline = Polyline . fmap (uncurry Vec2)
