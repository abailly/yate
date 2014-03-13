-- |Utilities for working with files and directories.
module Yate.Files where
import System.Directory(getDirectoryContents, doesFileExist, doesDirectoryExist,
                        getPermissions,setPermissions, Permissions(..))
import System.FilePath((</>))
import Control.Monad(filterM)
import Data.List(isPrefixOf)

-- |Filter non-hidden files
nonHiddenFiles :: FilePath -> Bool
nonHiddenFiles = (not . isPrefixOf ".")

-- |List files and directories from a list of filePath
filesAndDirs :: FilePath -> [FilePath] -> IO ([FilePath], [FilePath])
filesAndDirs source content = do
  files <- filterM (doesFileExist      . (source </>)) content
  dirs  <- filterM (doesDirectoryExist . (source </>)) content
  return (files,dirs)

-- |Make a file writable
ensureWritable :: FilePath -> IO FilePath
ensureWritable file = do
  p <- getPermissions file
  setPermissions file (p {writable = True})
  return file
  
-- |Ensure a directory and all its content is writable
--
-- This method exists because on Windows `git clone` checks out the .git directory in read-only mode which
-- makes deleting it fail
ensureWritableDir :: FilePath -> IO FilePath
ensureWritableDir dir = do
  _  <- ensureWritable dir
  content <- getDirectoryContents dir
  let nonHidden  = filter ( \ p -> p /= "." && p /= "..") content
  (files,dirs) <- filesAndDirs dir nonHidden
  mapM_ ensureWritableDir (map (dir </>) dirs)
  mapM_ ensureWritable (map (dir </> ) files)
  return dir

