{-# LANGUAGE OverloadedStrings #-}

-- | Command line parsers for options often used for penplotting.
module Draw.Plotting.CmdArgs (
      canvasP
    , Canvas(..)
) where



import Control.Monad
import Data.Foldable
import Options.Applicative
import Options.Applicative.Types



data Canvas = Canvas
    { _canvasWidth :: Double
    , _canvasHeight :: Double
    , _canvasMargin :: Double
    } deriving (Eq, Ord, Show)

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
    [ option customSizeReader $ mconcat
        [ long "size"
        , short 's'
        , metavar "[mm]"
        , help "Custom output size, format: <length>x<length>"
        ]
    , flag' (paper_a5_long, paper_a5_short) $ mconcat
        [ long "a5"
        , help "DIN A5 (210 mm × 148 mm)"
        ]
    , flag' (paper_a4_long, paper_a4_short) $ mconcat
        [ long "a4"
        , help "DIN A4 (271 mm × 210 mm)"
        ]
    , flag' (paper_a3_long, paper_a3_short) $ mconcat
        [ long "a3"
        , help "DIN A3 (420 mm × 271 mm)"
        ]
    , flag' (paper_a2_long, paper_a2_short) $ mconcat
        [ long "a2"
        , help "DIN A2 (594 mm × 420 mm)"
        ]
    , flag' (paper_a1_long, paper_a1_short) $ mconcat
        [ long "a1"
        , help "DIN A1 (841 mm × 594 mm)"
        ]
    ]

data Orientation = Landscape | Portrait deriving (Eq, Ord, Show)

orientationP :: Parser Orientation
orientationP = asum
    [ flag' Landscape $ mconcat
        [ long "landscape" ]
    , flag' Portrait $ mconcat
        [ long "landscape" ]
    ]

marginP :: Parser Double
marginP = option auto (mconcat
    [ long "margin"
    , metavar "[mm]"
    , value 10
    , showDefaultWith (\x -> show x <> " mm")
    , help "Ensure this much blank space to the edge"
    ])

paper_a5_short, paper_a5_long :: Double
paper_a4_short, paper_a4_long :: Double
paper_a3_short, paper_a3_long :: Double
paper_a2_short, paper_a2_long :: Double
paper_a1_short, paper_a1_long :: Double

paper_a5_short = 148
paper_a5_long = 210

paper_a4_short = paper_a5_long
paper_a4_long = 297

paper_a3_short = paper_a4_long
paper_a3_long = 420

paper_a2_short = paper_a3_long
paper_a2_long = 594

paper_a1_short = paper_a2_long
paper_a1_long = 841

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
