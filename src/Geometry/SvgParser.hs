{-# LANGUAGE OverloadedStrings #-}

module Geometry.SvgParser (parse, SvgElement(..)) where



import           Data.Text (Text)
import qualified Data.Text as T

import           Geometry.Bezier
import           Geometry.Core
import qualified Geometry.SvgParser.PathParser   as PathParser
import qualified Geometry.SvgParser.SimpleShapes as SimpleShapes



data SvgElement
    = SvgLine Line
    | SvgCircle Circle
    | SvgEllipse Ellipse
    | SvgPath [[Either Line Bezier]] -- ^ List of poly-bezier-lines.
    deriving (Show)

instance HasBoundingBox SvgElement where
    boundingBox (SvgLine x) = boundingBox x
    boundingBox (SvgCircle x) = boundingBox x
    boundingBox (SvgEllipse x) = boundingBox x
    boundingBox (SvgPath x) = boundingBox x

-- | Parse the input line-wise into various SVG elements.
--
-- * @<g>@ paths: @M413.654,295.115c0,0-1.283-13.865,12.717-19.615@
-- * Line elements: @LINE x1 y1 x2 y2@
-- * Circles: @CIRCLE cx cy r@
-- * Ellipses: @ELLIPSE cx cy rx ry@
--
-- This is not a full SVG parser, but in practice it’s much easier to edit an SVG
-- source file to extract all <g> element contents to a line rather than writing a
-- full-fledged SVG parser.
parse :: Text -> Either Text SvgElement
parse input = case PathParser.parse input of
    Right path -> Right (SvgPath path)
    Left pathErr -> case SimpleShapes.parse input of
        Right (SimpleShapes.SvgLine line) -> Right (SvgLine line)
        Right (SimpleShapes.SvgCircle circle) -> Right (SvgCircle circle)
        Right (SimpleShapes.SvgEllipse ellipse) -> Right (SvgEllipse ellipse)
        Left shapeErr -> Left (errorMsg input pathErr shapeErr)

errorMsg :: Text -> Text -> Text -> Text
errorMsg input pathErr shapeErr = T.unlines
    [ "None of the parsers succeeded for input:"
    , input
    , "All the different parser errors were:"
    , pathErr
    , shapeErr
    ]
