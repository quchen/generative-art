module VisualOutput.NormalizeSvg (normalizeSvgFile) where

import           Data.Foldable
import           Data.List
import           Data.Ord
import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           System.IO
import           Text.Regex.TDFA

import Util

-- | Cairo has nondeterministic output, since it seems to generate its running IDs from some shared counter.
-- This remedies that by renumbering all offending fields by time of occurrence in
-- the file.
normalizeSvgFile :: FilePath -> IO ()
normalizeSvgFile filename = modifyFileContent filename sanitizeSvgContent

findAllMatches :: Text -> Text -> [Text]
findAllMatches input regex = getAllTextMatches (input =~ regex)

modifyFileContent :: FilePath -> (Text -> Text) -> IO ()
modifyFileContent filename f = do
    content <- withFile filename ReadMode T.hGetContents
    withFile filename WriteMode $ \h -> T.hPutStr h (f content)

-- This has terrible performance because it copies the input file once for each unique nondeterministic
-- string, ugh.
sanitizeSvgContent :: Text -> Text
sanitizeSvgContent input
  = let nondeterministicStrings = findAllMatches input (T.pack "(surface|mask|clip|glyph[0-9]+-)[0-9]+")
        uniques = nubOrd nondeterministicStrings
        uniquesNumbered = zip uniques [1..]
        translationTable = [(unique, T.pack ("id" ++ show i)) | (unique, i) <- uniquesNumbered]

        -- We reverse the originals so we replace foo123 before foo1, which would yield be a collision
        reverseTranslationTable = sortOn (\(unique, _) -> Down unique) translationTable

        replace acc (original, replacement) = T.replace original replacement acc
    in foldl' replace input reverseTranslationTable
