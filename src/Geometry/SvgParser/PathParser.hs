{-# LANGUAGE OverloadedStrings #-}

-- | Parse an SVG path, as see in the <g> element, such as
-- @M413.654,295.115c0,0-1.283-13.865,12.717-19.615@.
module Geometry.SvgParser.PathParser (parse) where



import           Control.Applicative
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.Functor
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MPC

import Geometry.Bezier
import Geometry.Core
import Geometry.SvgParser.Common



vec2 :: Ord err => MP.Parsec err Text Vec2
vec2 = MP.label "position (x,y)" $ do
    x <- double
    MP.option () (char_ ',')
    y <- double
    pure (Vec2 x y)

data AbsRel = Absolute | Relative
    deriving (Eq, Ord, Show)

move :: Ord err => MP.Parsec err Text (Vec2 -> Start Vec2)
move = MP.label "move (mM)" $ do
    absRel <- Absolute <$ char_ 'M' <|> Relative <$ char_ 'm'
    v <- vec2
    pure $ case absRel of
        Absolute -> Start . const v
        Relative -> Start . (+. v)

data DrawState = DrawState
    { _startOfTrajectory :: Start Vec2
    , _currentPoint :: Current Vec2
    , _bezierReflection :: Maybe Vec2
    } deriving (Eq, Ord, Show)

-- Safety wrappers so I don’t mix them up
newtype Start a = Start a deriving (Eq, Ord, Show)
newtype Current a = Current a deriving (Eq, Ord, Show)

lineXY :: Ord err => MP.Parsec err Text (State DrawState Line)
lineXY = do
    absRel <- Absolute <$ char_ 'L' <|> Relative <$ char_ 'l'
    p <- vec2
    pure $ do
        DrawState start (Current current) _ <- get
        let new = case absRel of
                Absolute -> p
                Relative -> current +. p
        put (DrawState start (Current new) Nothing)
        pure (Line current new)

lineH :: Ord err => MP.Parsec err Text (State DrawState Line)
lineH = do
    absRel <- Absolute <$ char_ 'H' <|> Relative <$ char_ 'h'
    x' <- double
    pure $ do
        DrawState start (Current current@(Vec2 x y)) _ <- get
        let new = case absRel of
                Absolute -> Vec2 x' y
                Relative -> Vec2 (x+x') y
        put (DrawState start (Current new) Nothing)
        pure (Line current new)

lineV :: Ord err => MP.Parsec err Text (State DrawState Line)
lineV = do
    absRel <- Absolute <$ char_ 'V' <|> Relative <$ char_ 'v'
    y' <- double
    pure $ do
        DrawState start (Current current@(Vec2 x y)) _ <- get
        let new = case absRel of
                Absolute -> Vec2 x y'
                Relative -> Vec2 x (y+y')
        put (DrawState start (Current new) Nothing)
        pure (Line current new)

line :: Ord err => MP.Parsec err Text (State DrawState Line)
line = MP.label "line (lLhHvV)" $ lineXY <|> lineH <|> lineV

bezier :: Ord err => MP.Parsec err Text (State DrawState Bezier)
bezier = MP.label "cubical bezier (cCsS)" $ do
    (absRel, mirrorPreviousControlPoint) <- asum
        [ (Absolute, False) <$ char_ 'C'
        , (Relative, False) <$ char_ 'c'
        , (Absolute, True) <$ char_ 'S'
        , (Relative, True) <$ char_ 's'
        ]
    helper1 <- if mirrorPreviousControlPoint
        then pure . Left $ \current bezierReflection -> case bezierReflection of
            Nothing -> current -- SVG spec says to take the current point as fallback. Bit strange, but okay.
            Just mirrorMeOnCurrent ->
                let vecCurrentToP = mirrorMeOnCurrent -. current
                in current -. vecCurrentToP
        else fmap Right vec2
    helper2 <- vec2
    end <- vec2
    pure $ do
        DrawState start (Current current) bezierReflection <- get
        let [h1', h2', end'] = case absRel of
                -- In the absolute cases, we take what we get
                Absolute -> case helper1 of
                    Right h1 -> [h1, helper2, end]
                    Left fh1 -> [fh1 current bezierReflection, helper2, end]
                -- In the relative cases, we have to be careful not to apply the relative offset to the
                -- point already predetermined by the reflection
                Relative -> case helper1 of
                    Right h1 -> map (+. current) [h1, helper2, end]
                    Left fh1 -> fh1 current bezierReflection : map (+. current) [helper2, end]
        put (DrawState start (Current end') (Just h2'))

        pure (Bezier current h1' h2' end')

bezierCubic :: MP.Parsec Text Text a
bezierCubic = MP.label "" $ do
    _cubicChar <- asum (map char_ "qQtT")
    MP.customFailure "Quadratic bezier curves are not supported by the parser"

ellipticalArc :: MP.Parsec Text Text a
ellipticalArc = MP.label "" $ do
    _cubicChar <- asum (map char_ "aA")
    MP.customFailure "Elliptical arc curves are not supported by the parser"

closePath :: Ord err => MP.Parsec err Text (State DrawState Line)
closePath = MP.label "close path (zZ)" $ do
    char_ 'Z' <|> char_ 'z'
    pure $ do
        DrawState (Start start) (Current current) _ <- get
        put (DrawState (Start start) (Current start) Nothing) $> Line current start

parse :: Text -> Either Text [[Either Line Bezier]]
parse input = case MP.parse (MPC.space *> many parseSinglePathInstruction <* MP.eof) sourceFile input of
    Left errBundle -> Left (T.pack (MP.errorBundlePretty errBundle))
    Right pathInstructions -> Right (interpretAllDrawingInstructions pathInstructions)
  where
    sourceFile = ""

parseSinglePathInstruction :: MP.Parsec Text Text
    (
        Vec2 -> Start Vec2,                    -- Move to beginning, given the point the last drawing stopped (~M instruction)
        [State DrawState (Either Line Bezier)] -- Draw path
    )
parseSinglePathInstruction = do
    modifyStart <- move
    states <- MP.many $ asum
        [ (fmap.fmap) Left line
        , (fmap.fmap) Right bezier
        , bezierCubic
        , ellipticalArc
        ]
    maybeClosePath <- optional closePath

    let states' = case maybeClosePath of
            Nothing -> states
            Just closingLine -> states ++ [fmap Left closingLine]

    pure (modifyStart, states')

interpretSingleDrawingInstruction
    :: Traversable t
    => Vec2
    -> (Vec2 -> Start Vec2, t (State DrawState pathSegment))
    -> (t pathSegment, DrawState)
interpretSingleDrawingInstruction origin (moveToStart, steps) = runState (sequence steps) (DrawState (moveToStart origin) (let Start s = moveToStart origin in Current s) Nothing)

interpretAllDrawingInstructions :: [(Vec2 -> Start Vec2, [State DrawState pathSegment])] -> [[pathSegment]]
interpretAllDrawingInstructions = go (DrawState (Start zero) (Current zero) Nothing)
  where
    go _ [] = []
    go (DrawState (Start startPoint) _ _) (i:is) =
        let (path, state') = interpretSingleDrawingInstruction startPoint i
        in path : go state' is
