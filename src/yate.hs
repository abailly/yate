module Main where
import System.Environment(getArgs,getEnv)
import System.Directory(getDirectoryContents, doesFileExist, doesDirectoryExist,
                        createDirectoryIfMissing)
import System.FilePath((</>), takeFileName)
import Control.Monad(mplus, filterM)
import Data.List(isPrefixOf,intersperse)

type ProjectType = String

type TemplateInstantiator = String -> String

-- A node
data Node a = S a
            | L [ Tree a ]
              deriving (Eq, Show, Read)
                       
-- | A tree whose leafs are key/values pairs and nodes are labelled n-way trees.
data Tree a =  String :>: Node a
  deriving (Eq, Show, Read)

type ProjectDescription = Tree String

-- | Read a project description as a labelled tree.
--
-- >>> readDescription "\"b\" :>: S \"foo\""
-- "b" :>: S "foo"
--
-- >>> readDescription "\"project\" :>: L [ \"name\" :>: S \"myproj\"]"
-- "project" :>: L ["name" :>: S "myproj"]
--
-- >>> readDescription "\"a\" :>: L [ \"b\" :>: S \"foo\",  \"c\" :>: S \"bar\"]"
-- "a" :>: L ["b" :>: S "foo","c" :>: S "bar"]
readDescription :: String -> ProjectDescription
readDescription = read

data Path = K String
          | String :.: Path
            deriving (Eq, Show, Read)

-- |Build a path from a string description
--
-- >>> path "foo"
-- K "foo"
-- >>> path "foo.bar"
-- "foo" :.: K "bar"
-- >>> path "foo.bar.baz"
-- "foo" :.: ("bar" :.: K "baz")
path :: String -> Path
path input = case span (/= '.') input of
  (w,[])   -> K w
  (w,rest) -> w :.: path (tail rest)

-- | Extracts a single value from tree given a path
--
-- >>> select (K "bar") ("bar" :>: S "foo")
-- Just "foo"
-- >>> select (K "bar") ("baz" :>: S "foo")
-- Nothing
-- >>> select ("foo" :.: K "bar")  ("foo" :>: L ["bar" :>: S "baz" ])
-- Just "baz"
-- >>> select ("foo" :.: K "bar")  ("foo" :>: L ["qix" :>: S "foo", "bar" :>: S "baz" ])
-- Just "baz"
select :: Path -> Tree a -> Maybe a
select (K w)        (w' :>: S a) | w == w'    = Just a
select (w :.: rest) (w' :>: L l) | w == w'    = foldl (mplus) Nothing (map (select rest) l)
select _            _                        = Nothing

-- |Copy given file to target directory
copyFileTo :: FilePath -> TemplateInstantiator ->  FilePath -> IO FilePath
copyFileTo targetDirectory template source = do
  let f = targetDirectory </> takeFileName source
  input <- readFile source
  let output = template input
  writeFile f output
  return f

-- |Filter non-hidden files
nonHiddenFiles :: FilePath -> Bool
nonHiddenFiles = (not . isPrefixOf ".")

-- |List files and directories from a list of filePath
filesAndDirs :: FilePath -> [FilePath] -> IO ([FilePath], [FilePath])
filesAndDirs source content = do
  files <- filterM (doesFileExist      . (source </>)) content
  dirs  <- filterM (doesDirectoryExist . (source </>)) content
  return (files,dirs)

-- |Recursively copy a directory
copyDirectory :: FilePath                             -- ^Target directory
                 -> TemplateInstantiator              -- ^To instantiate template while copying files
                 -> (FilePath -> FilePath -> FilePath)  -- ^Mapping function, transforms filenames
                 -> FilePath                          -- ^Source directory
                 -> IO [FilePath]               
copyDirectory target template f source = do
  createDirectoryIfMissing True target
  content <- getDirectoryContents source
  let nonHidden  = filter nonHiddenFiles content
  (files,dirs) <- filesAndDirs source nonHidden
  newFiles <- mapM (copyFileTo target template . f source . (source </>)) files
  newDirs  <- mapM (\ d -> copyDirectory (target </>d ) template f (source </> d)) dirs
  return $ newFiles ++ (concat newDirs)

-- | Compute a mapping from source template files to instantiated template files
mapFiles :: FilePath   -- ^Template source directory
            -> ProjectDescription -- ^Project variables
            -> IO (FilePath -> FilePath -> FilePath)
mapFiles _ _ = return $ const id

-- |Locate the source directory for given project type
--
-- This should:
--
-- * locate environment variable YATE_TEMPLATES if it exists, and in this case use the directory
--   inside this directory that has same name than @projectType@
-- * Otherwise, try to download template through github, using current user as username/namespace
--   to use and template name as repository to clone, eg.
--     locateTemplate foo -> git clone https://github.com/user/foo 
locateTemplate :: ProjectType -> IO FilePath
locateTemplate projectType = do
  templatesDir <- getEnv "YATE_TEMPLATES"
  return $ templatesDir </> projectType
  
instantiateTemplate :: FilePath    -- ^Template source directory
                       -> FilePath -- ^Target instantiation directory
                       -> ProjectDescription -- ^Data to use for instantiating template variables
                       -> IO [FilePath]
instantiateTemplate sourceTemplate outputDirectory projectDescription = do
  fileMap <- mapFiles sourceTemplate projectDescription
  copyDirectory outputDirectory id fileMap sourceTemplate  
  
workToDo :: ProjectType           -- ^type of project, must resolve to source template
            -> FilePath           -- ^output directory of new project
            -> ProjectDescription -- ^project description
            -> IO [FilePath]
workToDo projectType outputDirectory projectDescription = do
  putStrLn $  unlines [ "Instantiating template from",
                        "\tdescription: " ++ show projectDescription,
                        "\ttype: " ++  projectType,
                        "\toutput: " ++ outputDirectory]
  templateDirectory <- locateTemplate projectType
  instantiateTemplate templateDirectory outputDirectory projectDescription

main :: IO ()
main = do
  [ projectType, outputDirectory, projectName ] <- getArgs
  let description = readDescription projectName
  created <- workToDo projectType outputDirectory description
  putStrLn "Copied Files: "
  putStrLn $ concat $ intersperse "\n" (map ("\t" ++) created)
