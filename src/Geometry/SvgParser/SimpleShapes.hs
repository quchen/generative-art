{-# LANGUAGE OverloadedStrings #-}

-- | Parse an SVG ellipse, as seen in the <line>, <circle> or <ellipse> element, e.g.
--
-- LINE x1=0 y1=80 x2=100 y2=20
-- CIRCLE cx=100 cy=50 r=100
-- ELLIPSE cx=100 cy=50 rx=100 ry=50
module Geometry.SvgParser.SimpleShapes (parse) where



import           Data.Foldable
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Text.Megaparsec      as MP
import qualified Text.Megaparsec.Char as MPC

import Geometry.Core
import Geometry.SvgParser.Common



data SimpleShape
    = SvgLine Line
    | SvgCircle Circle
    | SvgEllipse Ellipse
    deriving Show

parse :: Text -> Either Text SimpleShape
parse input = case MP.parse (MPC.space *> parseSimpleShape <* MP.eof) sourceFile input of
    Left errBundle -> Left (T.pack (MP.errorBundlePretty errBundle))
    Right shape -> Right shape
  where
    sourceFile = ""

parseSimpleShape :: MP.Parsec Text Text SimpleShape
parseSimpleShape = asum
    [ SvgLine <$> parseLine
    , SvgCircle <$> parseCircle
    , SvgEllipse <$> parseEllipse
    ]

parseLine :: MP.Parsec Text Text Line
parseLine = do
    _ <- lexeme (MPC.string "LINE")
    x1 <- assignedValue "x1"
    y1 <- assignedValue "y1"
    x2 <- assignedValue "x2"
    y2 <- assignedValue "y2"
    pure (Line (Vec2 x1 y1) (Vec2 x2 y2))

parseCircle :: MP.Parsec Text Text Circle
parseCircle = do
    _ <- lexeme (MPC.string "CIRCLE")
    cx <- assignedValue "cx"
    cy <- assignedValue "cy"
    r <- assignedValue "r"
    pure (Circle (Vec2 cx cy) r)

parseEllipse :: MP.Parsec Text Text Ellipse
parseEllipse = do
    _ <- lexeme (MPC.string "ELLIPSE")
    cx <- assignedValue "cx"
    cy <- assignedValue "cy"
    rx <- assignedValue "rx"
    ry <- assignedValue "ry"
    pure (Ellipse (translate (Vec2 cx cy) <> scale' rx ry))

assignedValue :: Text -> MP.Parsec Text Text Double
assignedValue name = do
    _ <- MPC.string name
    char_ '='
    double
