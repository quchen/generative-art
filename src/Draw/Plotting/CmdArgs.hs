-- | Command line parsers for options often used for penplotting.
module Draw.Plotting.CmdArgs (
      canvasP
    , Canvas(..)
) where



import Control.Monad
import Data.Foldable
import Draw.Plotting.PaperSize
import Geometry.Core
import Options.Applicative
import Options.Applicative.Types



data Canvas = Canvas
    { _canvasWidth :: Double
    , _canvasHeight :: Double
    , _canvasMargin :: Double
    } deriving (Eq, Ord, Show)

instance HasBoundingBox Canvas where
    boundingBox Canvas{_canvasWidth=w, _canvasHeight=h} = boundingBox [zero, Vec2 w h]

-- | Command line parser for common paper formats and orientations.
canvasP :: Parser Canvas
canvasP = f <$> paperSizeP <*> orientationP <*> marginP
  where
    f (wh1,wh2) orientation margin =
        let (w, h) = case orientation of
                Landscape -> (max wh1 wh2, min wh1 wh2)
                Portrait -> (min wh1 wh2, max wh1 wh2)
        in Canvas
            { _canvasWidth = w
            , _canvasHeight = h
            , _canvasMargin = margin
            }

paperSizeP :: Parser (Double, Double)
paperSizeP = asum
    [ flag' (paper_a1_long_mm, paper_a1_short_mm) $ mconcat
        [ long "a1"
        , help "DIN A1 (841 mm × 594 mm)"
        ]
    , flag' (paper_a2_long_mm, paper_a2_short_mm) $ mconcat
        [ long "a2"
        , help "DIN A2 (594 mm × 420 mm)"
        ]
    , flag' (paper_a3_long_mm, paper_a3_short_mm) $ mconcat
        [ long "a3"
        , help "DIN A3 (420 mm × 271 mm)"
        ]
    , flag' (paper_a4_long_mm, paper_a4_short_mm) $ mconcat
        [ long "a4"
        , help "DIN A4 (271 mm × 210 mm)"
        ]
    , flag' (paper_a5_long_mm, paper_a5_short_mm) $ mconcat
        [ long "a5"
        , help "DIN A5 (210 mm × 148 mm)"
        ]
    , option customSizeReader $ mconcat
        [ long "size"
        , short 's'
        , metavar "[mm]"
        , help "Custom output size, format: <length>x<length>"
        ]
    ]

data Orientation = Landscape | Portrait deriving (Eq, Ord, Show)

orientationP :: Parser Orientation
orientationP = asum
    [ flag' Landscape $ mconcat
        [ long "landscape" ]
    , flag' Portrait $ mconcat
        [ long "portrait" ]
    ]

marginP :: Parser Double
marginP = option auto (mconcat
    [ long "margin"
    , metavar "[mm]"
    , value 10
    , showDefaultWith (\x -> show x <> " mm")
    , help "Ensure this much blank space to the edge"
    ])

customSizeReader :: ReadM (Double, Double)
customSizeReader = do
    argStr <- readerAsk
    let wh = do
            (w, c:rest) <- reads argStr
            guard (c == 'x' || c == '×')
            (h, rest') <- reads rest
            guard (null rest')
            pure (w,h)
    case wh of
        [(w,h)] -> pure (w,h)
        _other -> readerError $ "Argument is not of the form <length>x<length>: " ++ argStr
