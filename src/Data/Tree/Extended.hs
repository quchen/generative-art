module Data.Tree.Extended (
      traverseTree
    , depthFirst
    , breadthFirst
    , module Data.Tree
) where



import           Data.List
import qualified Data.Sequence as Seq
import           Data.Tree



traverseTree
    :: (Tree a -> mem -> mem)       -- ^ Put a tree onto the to-visit list
    -> (mem -> Maybe (Tree a, mem)) -- ^ Get a tree from the to-visit list
    -> mem                          -- ^ To-visit list; initially the first node of the tree
    -> [a]
traverseTree put get memory = case get memory of
    Nothing -> []
    Just (Node x children, rest) ->
        x : traverseTree put get (foldl' (\acc y -> put y acc) rest children)

-- | Depth-first 'toList'. Probably the same as 'Foldable' 'Tree'.
depthFirst :: Tree a -> [a]
depthFirst tree = traverseTree putLeft getLeft (putLeft tree mempty)
  where
    putLeft = (:)
    getLeft [] = Nothing
    getLeft (x:xs) = Just (x,xs)

-- | Breadth-first 'toList'.
breadthFirst :: Tree a -> [a]
breadthFirst tree = traverseTree putLeft getRight (putLeft tree mempty)
  where
    putLeft = (Seq.<|)
    getRight xs = case Seq.viewr xs of
        Seq.EmptyR -> Nothing
        ys Seq.:> y -> Just (y, ys)
