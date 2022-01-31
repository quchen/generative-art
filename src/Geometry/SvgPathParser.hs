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

move :: Ord err => MP.Parsec err Text (State (Vec2, Vec2) ())
move = MP.label "move (mM)" $ do
    absRel <- Absolute <$ char_ 'M' <|> Relative <$ char_ 'm'
    p <- vec2
    pure $ do
        (_oldStart, current) <- get
        let new = case absRel of
                Absolute -> p
                Relative -> current +. p
        put (new, new)

lineXY :: Ord err => MP.Parsec err Text (State (Vec2, Vec2) Line)
lineXY = do
    absRel <- Absolute <$ char_ 'L' <|> Relative <$ char_ 'l'
    p <- vec2
    pure $ do
        (start, current) <- get
        let new = case absRel of
                Absolute -> p
                Relative -> current +. p
        put (start, new)
        pure (Line current new)

lineH :: Ord err => MP.Parsec err Text (State (Vec2, Vec2) Line)
lineH = do
    absRel <- Absolute <$ char_ 'H' <|> Relative <$ char_ 'h'
    x' <- double
    pure $ do
        (start, current@(Vec2 x y)) <- get
        let new = case absRel of
                Absolute -> Vec2 x' y
                Relative -> Vec2 (x+x') y
        put (start, new)
        pure (Line current new)

lineV :: Ord err => MP.Parsec err Text (State (Vec2, Vec2) Line)
lineV = do
    absRel <- Absolute <$ char_ 'V' <|> Relative <$ char_ 'v'
    y' <- double
    pure $ do
        (start, current@(Vec2 x y)) <- get
        let new = case absRel of
                Absolute -> Vec2 x y'
                Relative -> Vec2 x (y+y')
        put (start, new)
        pure (Line current new)

line :: Ord err => MP.Parsec err Text (State (Vec2, Vec2) Line)
line = MP.label "line (lLhHvV)" $ lineXY <|> lineH <|> lineV

bezier :: Ord err => MP.Parsec err Text (State (Vec2, Vec2) Bezier)
bezier = MP.label "cubical bezier (cC)" $ do
    absRel <- Absolute <$ char_ 'C' <|> Relative <$ char_ 'c'
    helper1 <- vec2
    helper2 <- vec2
    end <- vec2
    pure $ do
        (start, current) <- get
        let [h1', h2', end'] = case absRel of
                Absolute -> [helper1, helper2, end]
                Relative -> map (+. start) [helper1, helper2, end]
        put (start, end')
        pure (Bezier current h1' h2' end')

closePath :: Ord err => MP.Parsec err Text (State (Vec2, Vec2) (Maybe Line))
closePath = MP.label "close path (zZ)" $ do
    char_ 'Z' <|> char_ 'z'
    pure $ do
        (start, current) <- get
        if start /= current
            then put (start, start) *> pure (Just (Line current start))
            else pure Nothing

singlePath :: MP.Parsec Text Text [Either Line Bezier]
singlePath = do
    start <- move
    states <- MP.many $ (fmap.fmap) Left line <|> (fmap.fmap) Right bezier
    maybeClosePath <- optional closePath

    let (finished, _finalState) = flip runState (zero, zero) $ do
            start
            segments <- sequence states
            case maybeClosePath of
                Just drawClosingLine -> drawClosingLine >>= \case
                    Just closing -> pure (segments ++ [Left closing])
                    Nothing -> pure segments
                Nothing -> pure segments
    pure finished

instance MP.ShowErrorComponent Text where
    showErrorComponent = show

parse :: Text -> Either Text [[Either Line Bezier]]
parse input = case MP.parse (MPC.space *> many singlePath <* MP.eof) sourceFile input of
    Left errBundle -> Left (T.pack (MP.errorBundlePretty errBundle))
    Right path -> Right path
  where
    sourceFile = ""
