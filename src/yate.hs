{-# LANGUAGE DoAndIfThenElse #-}
module Main where
import System.Environment(getArgs,getEnv)
import System.Directory(getDirectoryContents, doesFileExist, doesDirectoryExist,
                        createDirectoryIfMissing,
                        getTemporaryDirectory)
import System.FilePath((</>), takeFileName,makeRelative)
import System.Process(rawSystem)
import Control.Monad(mplus, filterM)
import Data.List(isPrefixOf,intersperse)
import Text.Regex.PCRE((=~),compDotAll)
import Text.Regex.Base.RegexLike
import System.Posix.User(getLoginName)

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

-- |Select several subtrees of a tree.
--
-- >>>  selectMult (K "bar") ("bar" :>: L [ "foo" :>: S "bar", "foo" :>: S "baz" ])
-- ["foo" :>: S "bar","foo" :>: S "baz"]
-- >>>  selectMult ("bar" :.: K "foo") ("bar" :>: L [ "foo" :>: S "bar", "foo" :>: S "baz" ])
-- []
-- >>>  selectMult ("bar" :.: K "foo") ("bar" :>: L [ "foo" :>: L [ "baz" :>: S "bar", "foo" :>: S "baz" ]])
-- ["baz" :>: S "bar","foo" :>: S "baz"]
-- >>>  selectMult ("bar" :.: K "foo") ("bar" :>: L [ "foo" :>: L [ "baz" :>: S "bar"], "foo" :>: L [ "baz" :>: S "baz" ]])
-- ["baz" :>: S "bar","baz" :>: S "baz"]
selectMult :: Path -> Tree a -> [ Tree a ]
selectMult (K w)        (w' :>: L l) | w == w' = l
selectMult (w :.: rest) (w' :>: L l) | w == w' = foldl (mplus) [] (map (selectMult rest) l)
selectMult _      _                           = []

-- |Instantiate a template given some project description
--
-- Replaces all occurences of {{var}} with their value.
--
-- >>> instantiate ("foo" :>: L ["qix" :>: S "foo", "bar" :>: S "baz" ])  "foo"
-- "foo"
-- >>> instantiate ("foo" :>: L ["qix" :>: S "foo", "bar" :>: S "baz" ])  "{{foo.qix}}"
-- "foo"
-- >>> instantiate ("foo" :>: L ["qix" :>: S "foo", "bar" :>: S "baz" ])  "baz{{foo.qix}}bar"
-- "bazfoobar"
-- >>> instantiate ("foo" :>: L ["qix" :>: S "foo", "bar" :>: S "baz" ])  "baz{{foo.qix}}bar{{foo.bar}}"
-- "bazfoobarbaz"
-- >>> instantiate ("foo" :>: L ["qix" :>: S "foo", "bar" :>: S "baz" ])  "baz{{foo}}bar{{foo.bar}}"
-- "bazbarbaz"
instantiate :: ProjectDescription -> String -> String
instantiate project input = let (beg,found,end,subs) = input =~ "{{([^}]*)}}" :: (String, String, String, [String])
                            in  case found of
                              "" -> input
                              _  -> case select (path (head subs)) project of
                                Just v  -> beg ++ v ++ instantiate project end
                                Nothing -> beg ++      instantiate project end

-- |Instantiate list templates, eg. templates with multiple values
--
-- >>> instantiateMult ("foo" :>: L ["name" :>: S "foo", "name" :>: S "baz" ]) "{{foo.name}}{{#foo}}name: {{name}}\n{{/foo}}"
-- "fooname: foo\nname: baz\n"
-- >>> instantiateMult ("project" :>: L ["name" :>: S "foo", "authors" :>: L ["author" :>: S "baz" ]]) "{{#project.authors}}name: {{author}}\n{{/project.authors}}"
-- "name: baz\n"
instantiateMult :: ProjectDescription -> String -> String
instantiateMult project input = let regex = makeRegexOpts (defaultCompOpt + compDotAll) defaultExecOpt "{{#([^}]*)}}(.*){{/\\1}}"
                                    (beg,found,end,subs) = match regex input :: (String, String, String, [String])
                                in case found of
                                  "" -> input
                                  _  -> case selectMult (path (head subs)) project of
                                    []   -> instantiate project beg ++ instantiate project end
                                    rest -> instantiate project beg
                                            ++ concatMap (flip instantiate (head$tail subs)) rest
                                            ++ instantiateMult project end
                                      
-- |Copy given file to target directory
copyFileTo :: FilePath -> TemplateInstantiator ->  (FilePath -> FilePath) -> FilePath -> IO FilePath
copyFileTo targetDirectory template fileMap source = do
  let f = targetDirectory </> takeFileName (fileMap source)
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
  newFiles <- mapM (copyFileTo target template (f source) . (source </>)) files
  newDirs  <- mapM (\ d -> copyDirectory (target </>d ) template f (source </> d)) dirs
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
  instantiated <-  readFile mappings >>= return . instantiate project . instantiateMult project
  return $ makeFileMapper instantiated

cloneFromGithub :: ProjectType  -> IO FilePath
cloneFromGithub project = do
  tmp <- getTemporaryDirectory
  let uri = "https://github.com/" ++ project
  _ <- rawSystem "git" ["clone", uri, tmp </> project]
  return $ tmp </> project
  

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
-- * locate environment variable YATE_TEMPLATES if it exists, and in this case use the directory
--   inside this directory that has same name than @projectType@
-- * Otherwise, try to download template through github, using current user as username/namespace
--   to use and template name as repository to clone, eg.
--     locateTemplate foo -> git clone https://github.com/user/foo 
locateTemplate :: ProjectType -> IO FilePath
locateTemplate projectType = do
  templatesDir <- getEnv "YATE_TEMPLATES"
  let tmpl = templatesDir </> projectType
  localDir <- doesDirectoryExist tmpl
  if localDir then
     return tmpl
  else
     locateTemplateOnGitHub projectType
  
instantiateTemplate :: FilePath    -- ^Template source directory
                       -> FilePath -- ^Target instantiation directory
                       -> ProjectDescription -- ^Data to use for instantiating template variables
                       -> IO [FilePath]
instantiateTemplate sourceTemplate outputDirectory projectDescription = do
  fileMap <- mapFiles sourceTemplate projectDescription
  copyDirectory outputDirectory (instantiate projectDescription . instantiateMult projectDescription) fileMap sourceTemplate  
  
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
