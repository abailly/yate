{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}

-- | Handles actual instantation of files and directories from a template.
module Yate.Projects(makeTemplateInstance) where

import Control.Monad (when)
import System.Directory
       (createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents, getTemporaryDirectory, removeDirectoryRecursive)
import System.Environment (getEnv)
import System.Exit
import System.FilePath (makeRelative, makeValid, normalise, takeFileName, (</>))
import System.IO.Error (catchIOError)
import System.Posix.User (getLoginName)
import System.Process (rawSystem)

import Yate.Files
import Yate.Template
import Yate.Types

-- |Instantiate given template file from source directory to target directory
--
copyFileTo :: FilePath                    -- ^Target directory for the file
              -> TemplateInstantiator     -- ^Template instantiation to apply to the file
              ->  (FilePath -> FilePath)  -- ^File mapping
              -> FilePath                 -- ^Source directory
              -> IO FilePath              -- ^Instantiated file
copyFileTo targetDirectory template fileMap source = do
  let f = targetDirectory </> takeFileName (fileMap source)
  input <- readFile source
  let output = template input
  writeFile f output
  return f

-- |Recursively instantiate a template directory
copyDirectory :: FilePath                              -- ^Target directory
                 -> TemplateInstantiator               -- ^To instantiate template while copying files
                 -> (FilePath -> FilePath -> FilePath) -- ^Mapping function, transforms filenames
                 -> FilePath                           -- ^Source directory
                 -> IO [FilePath]
copyDirectory target template f source = do
  createDirectoryIfMissing True target
  content <- getDirectoryContents source
  let nonHidden  = filter nonHiddenFiles content
  (files,dirs) <- filesAndDirs source nonHidden
  newFiles <- mapM (copyFileTo target template (f source) . (source </>)) files
  newDirs  <- mapM (\ d -> copyDirectory (target </> d) template f (source </> d)) dirs
  return $ newFiles ++ (concat newDirs)


type FileMapping = (FilePath,FilePath)

makeFileMapper :: String -> (FilePath -> FilePath -> FilePath)
makeFileMapper input = let mappings = read input :: [FileMapping]
                           mapping sourceDir file =
                             let relativeFile = makeRelative sourceDir file
                             in case lookup relativeFile mappings of
                               Nothing -> relativeFile
                               Just f  -> f
                       in mapping

-- | Compute a mapping from source template files to instantiated template files
mapFiles :: FilePath   -- ^Template source directory
            -> ProjectDescription -- ^Project variables
            -> IO (FilePath -> FilePath -> FilePath)
mapFiles source project = do
  let mappings =  source </> ".mapping"
  instantiated <- readFile mappings >>= return . doTemplate project
  return $ makeFileMapper instantiated


cloneFromGithub :: ProjectType  -> IO FilePath
cloneFromGithub project = do
  tmp <- getTemporaryDirectory
  let output = makeValid $ normalise $ tmp </> project
  exist <- (doesDirectoryExist output)
  when exist $ ensureWritableDir output >>= removeDirectoryRecursive
  let uri = "https://github.com/" ++ project
  e <- rawSystem "git" ["clone", uri, tmp </> project]
  case e of
    ExitFailure code -> ioError (userError $ "failed to clone " ++ uri ++ " to " ++ output ++ ", exit code: " ++ show code)
    ExitSuccess      -> return $ tmp </> project


locateTemplateOnGitHub :: ProjectType -> IO FilePath
locateTemplateOnGitHub project =
  if '/' `elem` project then
     cloneFromGithub project
  else
     getLoginName >>= \ user -> cloneFromGithub (user ++ "/" ++ project)

-- |Locate the source directory for given project type
--
-- This should:
--
-- * locate environment variable @YATE_TEMPLATES@ if it exists, and in this case use the directory
--   inside this directory that has same name than @projectType@
-- * Otherwise, try to download template through github, using current user as username/namespace
--   to use and template name as repository to clone, eg.
-- @@
--     locateTemplate foo -> git clone https://github.com/user/foo
-- @@
locateTemplate :: ProjectType -> IO FilePath
locateTemplate projectType = do
  templatesDir <- catchIOError (getEnv "YATE_TEMPLATES") (\ _ -> return "none")
  let tmpl = templatesDir </> projectType
  localDir <- doesDirectoryExist tmpl
  if localDir then
    return tmpl
  else
    locateTemplateOnGitHub projectType

instantiateTemplate :: FilePath           -- ^Template source directory
                    -> FilePath           -- ^Target instantiation directory
                    -> ProjectDescription -- ^Data to use for instantiating template variables
                    -> IO [FilePath]
instantiateTemplate sourceTemplate outputDirectory projectDescription = do
  fileMap <- mapFiles sourceTemplate projectDescription
  copyDirectory outputDirectory (doTemplate projectDescription) fileMap sourceTemplate

-- |Instantiate directory structure from a given source project type.
-- The source of the template is first located, either from the local file-system or through
-- cloning from Github. Then it is instantiated using the given project descriptor replicating
-- the directory structure according to the .mapping template.
--
-- Returns the list of created files.
makeTemplateInstance :: ProjectType        -- ^type of project, must resolve to source template
                     -> FilePath           -- ^output directory of new project
                     -> ProjectDescription -- ^project description
                     -> IO [FilePath]
makeTemplateInstance projectType outputDirectory projectDescription = do
  putStrLn $  unlines [ "Instantiating template from",
                        "\tdescription: " ++ show projectDescription,
                        "\ttype: " ++  projectType,
                        "\toutput: " ++ outputDirectory]
  templateDirectory <- locateTemplate projectType
  instantiateTemplate templateDirectory outputDirectory projectDescription
