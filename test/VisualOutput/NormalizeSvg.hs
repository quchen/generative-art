module VisualOutput.NormalizeSvg (normalizeSvgFile) where

import           Data.Foldable
import           Data.List
import           Data.Ord
import qualified Data.Set        as S
import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           System.IO
import           Text.Regex.TDFA

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

nub' :: Ord a => [a] -> [a]
nub' = go S.empty
  where
    go _ [] = []
    go seen (x:xs)
        | x `S.member` seen = go seen xs
        | otherwise = x : go (S.insert x seen) xs

-- This has terrible performance because it copies the input file once for each unique nondeterministic
-- string, ugh.
sanitizeSvgContent :: Text -> Text
sanitizeSvgContent input
  = let nondeterministicStrings = findAllMatches input (T.pack "(surface|mask|clip|glyph[0-9]+-)[0-9]+")
        uniques = nub' nondeterministicStrings
        uniquesNumbered = zip uniques [1..]
        translationTable = [(unique, T.pack ("id" ++ show i)) | (unique, i) <- uniquesNumbered]

        -- We reverse the originals so we replace foo123 before foo1, which would yield be a collision
        reverseTranslationTable = sortOn (\(unique, _) -> Down unique) translationTable

        replace acc (original, replacement) = T.replace original replacement acc
    in foldl' replace input reverseTranslationTable
