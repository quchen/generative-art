module VisualOutput.FileSystem (
    listAllFiles
) where

import Data.List
import Data.Traversable
import System.Directory
import System.FilePath

data FileType = RegularFile | Directory | Other | NotFound
    deriving (Eq, Ord, Show)

fileType :: FilePath -> IO FileType
fileType path = ifM
    [ (doesFileExist path,      RegularFile)
    , (doesDirectoryExist path, Directory)
    , (doesPathExist path,      Other)
    , (pure otherwise,          NotFound)
    ]

ifM :: Monad m => [(m Bool, a)] -> m a
ifM [] = error "ifM: non-exhaustive list of conditions"
ifM ((conditionM, result):xs) = conditionM >>= \case
    True -> pure result
    False -> ifM xs

-- | List the immediate contents of a directory. Unsafe because it throws an
-- exception if the input is not a directory.
unsafeListDir :: FilePath -> IO [FilePath]
unsafeListDir dir = do
    filesInDir <- do
        basenames <- listDirectory dir
        pure [dir </> basename | basename <- sort basenames]
    pure filesInDir

listAllFiles :: FilePath -> IO [FilePath]
listAllFiles file = fileType file >>= \case
    RegularFile -> pure [file]
    Directory -> do
        contained <- unsafeListDir file
        recursed <- for contained listAllFiles
        pure (concat recursed)
    Other -> pure []
    NotFound -> pure []
