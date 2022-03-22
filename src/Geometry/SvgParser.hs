{-# LANGUAGE OverloadedStrings #-}

module Geometry.SvgParser (parse) where



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
    | SvgPath [[Either Line Bezier]]

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
