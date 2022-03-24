{-# OPTIONS_GHC -fno-warn-orphans #-}

module Geometry.SvgParser.Common (
      lexeme
    , double
    , char_
) where



import           Control.Applicative
import           Data.Functor
import           Data.Text                  (Text)
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCLex



instance MP.ShowErrorComponent Text where
    showErrorComponent = show

-- | Run a parser, and discard any whitespace after it.
lexeme :: Ord err => MP.Parsec err Text a -> MP.Parsec err Text a
lexeme  = MP.label "" . MPCLex.lexeme MPC.space

double :: Ord err => MP.Parsec err Text Double
double = MP.label "number" $ lexeme $ MPCLex.signed (pure ()) $
    MP.try MPCLex.float <|> fmap fromIntegral MPCLex.decimal

char_ :: Ord err => Char -> MP.Parsec err Text ()
char_ c = lexeme (MPC.char c) $> ()
