{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Geometry.SvgPathParser (parse) where



import           Control.Applicative
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCLex

import Geometry.Bezier
import Geometry.Core



lexeme :: Ord err => MP.Parsec err Text a -> MP.Parsec err Text a
lexeme  = MP.label "" . MPCLex.lexeme MPC.space

double :: Ord err => MP.Parsec err Text Double
double = MP.label "number" $ lexeme $ MPCLex.signed (pure ()) $
    MP.try MPCLex.float <|> fmap fromIntegral MPCLex.decimal

char_ :: Ord err => Char -> MP.Parsec err Text ()
char_ c = lexeme (MPC.char c) *> pure ()

vec2 :: Ord err => MP.Parsec err Text Vec2
vec2 = Vec2 <$> double <*> double

data AbsRel = Absolute | Relative
    deriving (Eq, Ord, Show)

move :: Ord err => MP.Parsec err Text (Vec2 -> Vec2)
move = MP.label "move (mM)" $ do
    absRel <- Absolute <$ char_ 'M' <|> Relative <$ char_ 'm'
    v <- vec2
    pure $ case absRel of
        Absolute -> const v
        Relative -> (+. v)

data DrawState = DrawState
    { _startOfTrajectory :: Vec2
    , _currentPoint :: Vec2
    } deriving (Eq, Ord, Show)



lineXY :: Ord err => MP.Parsec err Text (State DrawState Line)
lineXY = do
    absRel <- Absolute <$ char_ 'L' <|> Relative <$ char_ 'l'
    p <- vec2
    pure $ do
        DrawState start current <- get
        let new = case absRel of
                Absolute -> p
                Relative -> current +. p
        put (DrawState start new)
        pure (Line current new)

lineH :: Ord err => MP.Parsec err Text (State DrawState Line)
lineH = do
    absRel <- Absolute <$ char_ 'H' <|> Relative <$ char_ 'h'
    x' <- double
    pure $ do
        DrawState start current@(Vec2 x y) <- get
        let new = case absRel of
                Absolute -> Vec2 x' y
                Relative -> Vec2 (x+x') y
        put (DrawState start new)
        pure (Line current new)

lineV :: Ord err => MP.Parsec err Text (State DrawState Line)
lineV = do
    absRel <- Absolute <$ char_ 'V' <|> Relative <$ char_ 'v'
    y' <- double
    pure $ do
        DrawState start current@(Vec2 x y) <- get
        let new = case absRel of
                Absolute -> Vec2 x y'
                Relative -> Vec2 x (y+y')
        put (DrawState start new)
        pure (Line current new)

line :: Ord err => MP.Parsec err Text (State DrawState Line)
line = MP.label "line (lLhHvV)" $ lineXY <|> lineH <|> lineV

bezier :: Ord err => MP.Parsec err Text (State DrawState Bezier)
bezier = MP.label "cubical bezier (cC)" $ do
    absRel <- Absolute <$ char_ 'C' <|> Relative <$ char_ 'c'
    helper1 <- vec2
    helper2 <- vec2
    end <- vec2
    pure $ do
        DrawState start current <- get
        let [h1', h2', end'] = case absRel of
                Absolute -> [helper1, helper2, end]
                Relative -> map (+. start) [helper1, helper2, end]
        put (DrawState start end')

        pure (Bezier current h1' h2' end')

bezierCubic :: MP.Parsec Text Text a
bezierCubic = MP.label "" $ do
    _cubicChar <- asum (map char_ "qQtT")
    MP.customFailure ("Quadratic bezier curves are not supported by the parser")

ellipticalArc :: MP.Parsec Text Text a
ellipticalArc = MP.label "" $ do
    _cubicChar <- asum (map char_ "aA")
    MP.customFailure ("Elliptical arc curves are not supported by the parser")

closePath :: Ord err => MP.Parsec err Text (State DrawState Line)
closePath = MP.label "close path (zZ)" $ do
    char_ 'Z' <|> char_ 'z'
    pure $ do
        DrawState start current <- get
        put (DrawState start start) *> pure (Line current start)

instance MP.ShowErrorComponent Text where
    showErrorComponent = show

parse :: Text -> Either Text [[Either Line Bezier]]
parse input = case MP.parse (MPC.space *> many parseSinglePathInstruction <* MP.eof) sourceFile input of
    Left errBundle -> Left (T.pack (MP.errorBundlePretty errBundle))
    Right pathInstructions -> Right $ interpretAllDrawingInstructions pathInstructions
  where
    sourceFile = ""

parseSinglePathInstruction :: MP.Parsec Text Text
    (
        Vec2 -> Vec2,                          -- Move to beginning
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
    -> (Vec2 -> Vec2, t (State DrawState pathSegment))
    -> (t pathSegment, DrawState)
interpretSingleDrawingInstruction origin (moveToStart, steps) = runState (sequence steps) (DrawState (moveToStart origin) (moveToStart origin))

interpretAllDrawingInstructions :: [(Vec2 -> Vec2, [State DrawState pathSegment])] -> [[pathSegment]]
interpretAllDrawingInstructions = go (DrawState zero zero)
  where
    go _ [] = []
    go (DrawState _ startPoint) (i:is) =
        let (path, state') = interpretSingleDrawingInstruction startPoint i
        in path : go state' is
