{-# LANGUAGE OverloadedStrings #-}

-- | Parse an SVG path, as see in the <g> element, such as
-- @M413.654,295.115c0,0-1.283-13.865,12.717-19.615@.
module Geometry.SvgParser.PathParser (parse) where



import           Control.Applicative
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.Maybe
import           Data.Text                 (Text)
import           Data.Traversable
import qualified Data.Text                 as T
import qualified Text.Megaparsec           as MP
import qualified Text.Megaparsec.Char      as MPC

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

move :: Ord err => MP.Parsec err Text (State DrawState [Line])
move = MP.label "move (mM)" $ do
    absRel <- Absolute <$ char_ 'M' <|> Relative <$ char_ 'm'
    mM:vs <- someMaybeCommaSep vec2
    pure $ do
        Current oldCurrent <- gets _currentPoint
        let newStart = case absRel of
                Absolute -> mM
                Relative -> oldCurrent +. mM
        modify' $ \s -> s
            { _startOfTrajectory = Start newStart
            , _currentPoint = Current newStart
            , _bezierReflectionQuadratic = Nothing
            , _bezierReflectionCubic = Nothing }
        for vs (makeLine absRel)

data DrawState = DrawState
    { _startOfTrajectory :: Start Vec2
    , _currentPoint :: Current Vec2
    , _bezierReflectionQuadratic :: Maybe Vec2
    , _bezierReflectionCubic :: Maybe Vec2
    } deriving (Eq, Ord, Show)

-- Safety wrappers so I don’t mix them up
newtype Start a = Start a deriving (Eq, Ord, Show)
newtype Current a = Current a deriving (Eq, Ord, Show)

lineXY :: Ord err => MP.Parsec err Text (State DrawState [Line])
lineXY = do
    absRel <- Absolute <$ char_ 'L' <|> Relative <$ char_ 'l'
    ps <- someMaybeCommaSep vec2
    pure $ traverse (makeLine absRel) ps

makeLine :: AbsRel -> Vec2 -> State DrawState Line
makeLine absRel p = do
    Current current <- gets _currentPoint
    let new = case absRel of
            Absolute -> p
            Relative -> current +. p
    modify' $ \s -> s
        { _currentPoint = Current new
        , _bezierReflectionQuadratic = Nothing
        , _bezierReflectionCubic = Nothing }
    pure (Line current new)

lineH :: Ord err => MP.Parsec err Text (State DrawState [Line])
lineH = do
    absRel <- Absolute <$ char_ 'H' <|> Relative <$ char_ 'h'
    xs <- someMaybeCommaSep double
    pure $ for xs $ \x' -> do
        Current current@(Vec2 x y) <- gets _currentPoint
        let new = case absRel of
                Absolute -> Vec2 x' y
                Relative -> Vec2 (x+x') y
        modify' $ \s -> s
            { _currentPoint = Current new
            , _bezierReflectionQuadratic = Nothing
            , _bezierReflectionCubic = Nothing }
        pure (Line current new)

lineV :: Ord err => MP.Parsec err Text (State DrawState [Line])
lineV = do
    absRel <- Absolute <$ char_ 'V' <|> Relative <$ char_ 'v'
    ys <- someMaybeCommaSep double
    pure $ for ys $ \y' -> do
        Current current@(Vec2 x y) <- gets _currentPoint
        let new = case absRel of
                Absolute -> Vec2 x y'
                Relative -> Vec2 x (y+y')
        modify' $ \s -> s
            { _currentPoint = Current new
            , _bezierReflectionQuadratic = Nothing
            , _bezierReflectionCubic = Nothing }
        pure (Line current new)

line :: Ord err => MP.Parsec err Text (State DrawState [Line])
line = MP.label "line (lLhHvV)" $ lineXY <|> lineH <|> lineV

bezierCubic :: Ord err => MP.Parsec err Text (State DrawState [Bezier])
bezierCubic = MP.label "cubical bezier (cCsS)" $ do
    (absRel, mirrorPreviousControlPoint) <- asum
        [ (Absolute, False) <$ char_ 'C'
        , (Relative, False) <$ char_ 'c'
        , (Absolute, True) <$ char_ 'S'
        , (Relative, True) <$ char_ 's'
        ]
    controlPoints <- someMaybeCommaSep $ case mirrorPreviousControlPoint of
        False -> Left <$> ((,,) <$> vec2 <*> vec2 <*> vec2)
        True -> Right <$> ((,) <$> vec2 <*> vec2)
    pure $ for controlPoints $ \cps -> do
        DrawState{_currentPoint = Current current, _bezierReflectionCubic = reflectMe} <- get
        curve@(Bezier _ _ helper2 end) <- case cps of
            Left (helper1Raw, helper2Raw, endRaw) -> do
                let (helper1, helper2, end) = case absRel of
                        Absolute -> (helper1Raw, helper2Raw, endRaw)
                        Relative -> (helper1Raw +. current, helper2Raw +. current, endRaw +. current)
                pure (Bezier current helper1 helper2 end)
            Right (helper2Raw, endRaw) -> do
                let helper1 = current -. fromMaybe current reflectMe
                    (helper2, end) = case absRel of
                        Absolute -> (helper2Raw, endRaw)
                        Relative -> (helper2Raw +. current, endRaw +. current)
                pure (Bezier current helper1 helper2 end)
        modify' $ \s -> s
            { _currentPoint = Current end
            , _bezierReflectionCubic = Just helper2
            , _bezierReflectionQuadratic = Nothing }
        pure curve

bezierQuadratic :: Ord err => MP.Parsec err Text (State DrawState [Bezier])
bezierQuadratic = MP.label "quadratic bezier (qQtT)" $ do
    (absRel, mirrorPreviousControlPoint) <- asum
        [ (Absolute, False) <$ char_ 'Q'
        , (Relative, False) <$ char_ 'q'
        , (Absolute, True) <$ char_ 'T'
        , (Relative, True) <$ char_ 't'
        ]
    controlPoints <- someMaybeCommaSep $ case mirrorPreviousControlPoint of
        False -> Left <$> ((,) <$> vec2 <*> vec2)
        True -> Right <$> vec2
    pure $ for controlPoints $ \cps -> do
        DrawState{_currentPoint = Current current, _bezierReflectionQuadratic = reflectMe} <- get
        curve@(QuadraticBezier _ helper end) <- case cps of
            Left (helperRaw, endRaw) -> do
                let (helper, end) = case absRel of
                        Absolute -> (helperRaw, endRaw)
                        Relative -> (helperRaw +. current, endRaw +. current)
                pure (QuadraticBezier current helper end)
            Right endRaw -> do
                let helper = current -. fromMaybe current reflectMe
                    end = case absRel of
                        Absolute -> endRaw
                        Relative -> endRaw +. current
                pure (QuadraticBezier current helper end)
        modify' $ \s -> s
            { _currentPoint = Current end
            , _bezierReflectionQuadratic = Just helper
            , _bezierReflectionCubic = Nothing }
        pure (quadraticToCubical curve)

data QuadraticBezier = QuadraticBezier Vec2 Vec2 Vec2 deriving (Eq, Ord, Show)

quadraticToCubical :: QuadraticBezier -> Bezier
quadraticToCubical (QuadraticBezier qStart qHelper qEnd) =
    let cStart = qStart
        cHelper1 = qStart +. 2/3 *. (qHelper -.qStart)
        cHelper2 = qEnd   +. 2/3 *. (qHelper -.qEnd)
        cEnd = qEnd
    in Bezier cStart cHelper1 cHelper2 cEnd

bezier :: Ord err => MP.Parsec err Text (State DrawState [Bezier])
bezier = bezierQuadratic <|> bezierCubic

ellipticalArc :: MP.Parsec Text Text a
ellipticalArc = MP.label "" $ do
    _cubicChar <- asum (map char_ "aA")
    MP.customFailure "Elliptical arc curves are not supported by the parser"

closePath :: Ord err => MP.Parsec err Text (State DrawState Line)
closePath = MP.label "close path (zZ)" $ do
    char_ 'Z' <|> char_ 'z'
    pure $ do
        DrawState {_startOfTrajectory = Start start, _currentPoint = Current current} <- get
        modify' $ \s -> s {_currentPoint = Current start, _bezierReflectionQuadratic = Nothing, _bezierReflectionCubic = Nothing }
        pure (Line current start)

parse :: Text -> Either Text [[Either Line Bezier]]
parse input = case MP.parse (MPC.space *> many parseSinglePathInstruction <* MP.eof) sourceFile input of
    Left errBundle -> Left (T.pack (MP.errorBundlePretty errBundle))
    Right pathInstructions -> Right (interpretAllDrawingInstructions pathInstructions)
  where
    sourceFile = ""

parseSinglePathInstruction :: MP.Parsec Text Text [State DrawState (Either [Line] [Bezier])]
parseSinglePathInstruction = do
    moveLines <- (fmap.fmap) Left move
    states <- MP.many $ asum
        [ (fmap.fmap) Left line
        , (fmap.fmap) Right bezier
        , ellipticalArc
        ]
    closePathLine <- optional closePath >>= \case
        Nothing -> pure []
        Just close -> pure [fmap (\x -> Left [x]) close]

    pure (moveLines : states ++ closePathLine)

interpretAllDrawingInstructions :: [[State DrawState (Either [Line] [Bezier])]] -> [[Either Line Bezier]]
interpretAllDrawingInstructions instructions = evalState (go instructions) initialState
  where
    initialState = DrawState
        { _startOfTrajectory = Start zero
        , _currentPoint = Current zero
        , _bezierReflectionQuadratic = Nothing
        , _bezierReflectionCubic = Nothing
        }

    go :: [[State DrawState (Either [Line] [Bezier])]] -> State DrawState [[Either Line Bezier]]
    go [] = pure []
    go (is:iss) = do
        paths <- fmap flatten (sequence is)
        rest <- go iss
        pure (paths : rest)

    flatten :: [Either [Line] [Bezier]] -> [Either Line Bezier]
    flatten [] = []
    flatten (Left ls : xs) = map Left ls ++ flatten xs
    flatten (Right bs : xs) = map Right bs ++ flatten xs
