module Data.Multwomap (
    Multwomap()
    , empty
    , null
    , size
    , union
    , insert
    , arbitraryKey
    , extract
) where



import           Data.Map (Map)
import qualified Data.Map as M
import           Prelude  hiding (null)



data OneTwo a = One a | Two a a deriving (Eq, Ord, Show)

-- | A multimap where each key can have at most two entries. Note that this is an
-- unsafe data structure: attempting to add a key twice will crash the program.
newtype Multwomap k v = Multwomap (Map k (OneTwo v)) deriving (Eq, Ord, Show)

empty :: Multwomap k v
empty = Multwomap M.empty

null :: Multwomap k v -> Bool
null (Multwomap mmap) = M.null mmap

size :: Multwomap k v -> Int
size (Multwomap mmap) = sum (M.map (\case One{} -> 1; Two{} -> 2) mmap)

insert :: (Ord k, Eq v) => k -> v -> Multwomap k v -> Either String (Multwomap k v)
insert k v (Multwomap mmap) = case M.lookup k mmap of
    Nothing -> Right (Multwomap (M.insert k (One v) mmap))
    Just old -> case mergeOneTwo old (One v) of
        Right new -> Right (Multwomap (M.insert k new mmap))
        Left err  -> Left ("Multwomap: " ++ err)

union :: (Ord k, Eq v) => Multwomap k v -> Multwomap k v -> Either String (Multwomap k v)
union (Multwomap mmap1) (Multwomap mmap2) =
    Multwomap <$> M.foldrWithKey combine (Right mmap1) mmap2
  where
    combine k new (Right acc) = case M.lookup k acc of
        Nothing   -> Right (M.insert k new acc)
        Just old  -> case mergeOneTwo old new of
            Right merged -> Right (M.insert k merged acc)
            Left err      -> Left ("Multwomap: " ++ err)
    combine _ _ (Left err) = Left err

-- | Combine two entries for the same key. The data structure's invariant is
-- that a key maps to at most two /distinct/ values (one fragment entering,
-- one leaving a vertex, in the Margalit–Knott polygon-clipping algorithm).
--
-- Merging is /idempotent/: adding the same value twice does not count as a
-- second distinct target — 'Two a a' collapses to 'One a'. This is essential
-- when both input polygons contribute the *same* shared boundary edge
-- fragment: each polygon's edge-fragment map adds @x -> y@, and their union
-- would otherwise produce a spurious 'Two' that walks the shared edge twice.
--
-- A genuine overflow — three /distinct/ values for one key — is reported as a
-- 'Left' so the caller can attach context (e.g. the input polygons that
-- triggered the invariant violation).
mergeOneTwo :: Eq a => OneTwo a -> OneTwo a -> Either String (OneTwo a)
mergeOneTwo (One a) (One b)
    | a == b    = Right (One a)
    | otherwise = Right (Two a b)
mergeOneTwo Two{}   Two{}   = Left "Overflow: both args already have two targets"
mergeOneTwo (Two a b) (One c)
    | a == c || b == c = Right (Two a b)
    | otherwise        = Left "Overflow: first arg already has two targets"
mergeOneTwo (One c) (Two a b)
    | a == c || b == c = Right (Two a b)
    | otherwise        = Left "Overflow: second arg already has two targets"

-- | Get an arbitrary key contained in the Multwomap, or 'Nothing' if it’s empty.
arbitraryKey :: Multwomap k v -> Maybe k
arbitraryKey (Multwomap mmap) = fmap fst (M.lookupMin mmap)

-- | Extract the value to the corresponding key, and return the Multwomap with the
-- key removed.
extract :: Ord k => k -> Multwomap k v -> Maybe (v, Multwomap k v)
extract k (Multwomap mmap) = case M.lookup k mmap of
    Nothing -> Nothing
    Just (One v) -> pure (v, Multwomap (M.delete k mmap))
    Just (Two v w) -> pure (v, Multwomap (M.insert k (One w) mmap))
