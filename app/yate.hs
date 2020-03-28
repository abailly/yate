{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE CPP #-}

import System.Directory(doesFileExist)
import System.Environment(getArgs)
import System.Console.GetOpt
import Data.List(intersperse)

import Yate.Projects
import Yate.Template
import Yate.Types


headline :: String
headline = "Yet Another Template Engine, Copyright (c) 2014 Arnaud Bailly <arnaud.oqube@gmail.com>\n\
Usage: yate [OPTIONS..] [PROJECT DESCRIPTOR]\n"

usage :: String
usage  =  "Instantiate the given <source template> in the <target directory> using variables provided by <project descriptor>.\n\
Descriptor can be given as a single argument on the command-line or as a file name to be read.\n\
\n\
Details\n\
=======\n\
\n\
* Source template is a name that is resolved in fine to a directory on the local system. This resolution can\n\
  either be:\n\
   - Locating a directory named <source template> under a directory pointed at by YATE_TEMPLATES environment variable\n\
   - Cloning to a temporary directory a Github project named <current user>/<source template> iff <source template> \n\
     does not contain a '/', otherwise use the full <source template> name for Github reference\n\
* Target directory is where the instantiated templates will be output. If it does not exist, it is created\n\
* Project descriptor is an arbitrary JSON object whose fields are used to instantiate variables referenced in the \n\
  source template\n\
\n\
Templates\n\
=========\n\
\n\
Templates are simple Mustache (http://mustache.github.io) templates with <project descriptor> used as context for \n\
variables resolution:\n\
\n\
* {{foo.bar}} is a reference to a single value and is instantiated with field bar in field foo from context\n\
* {{#foo}}...{{/foo}} is a block which is instantiated once for each value in the foo collection. The content of \n\
  the block uses the passed value as context for resolution\n\
\n\
A template can contain a .mapping file at its root which maps source files to target files. This file is itself\n\
a template that is instantiated with the descriptor prior to be used for file names mapping. After template \n\
instantiation this file should be readable Haskell list of string tuples (eg. have type [(String,String)]): \n\
Use [ and ] to enclose the list, separate each item with a comma, use ( and ) to enclose pairs of source/target\n\
file names."

data YateConfig = YateConfig {
  yateProjectType       :: ProjectType,  -- ^Type of project, references some template
  yateOutputDirectory   :: FilePath,     -- ^Output of instantiated project
  yateProjectDescriptor :: FilePath,     -- ^Project descriptor, eg. context for resolving variables
  yateHelp              :: Bool
  } deriving (Eq,Show,Read)

defaultConfig :: YateConfig
defaultConfig = YateConfig {
  yateProjectType = "haskell",
  yateOutputDirectory = "project",
  yateHelp = False,
  yateProjectDescriptor = "project.json"
  }
                
options :: [OptDescr (YateConfig -> YateConfig)]
options =
     [ Option ['h','?']     ["help"]
         (NoArg (\ opts -> opts { yateHelp = True }))
         "yate usage and help"
     , Option ['o']     ["output"]
         (ReqArg (\ f opts -> opts { yateOutputDirectory = f })
                 "DIRECTORY")
         "output DIRECTORY"
     , Option ['t']     ["type"]
         (ReqArg (\ f opts -> opts { yateProjectType =  f })
                 "STRING")
         "type STRING"
     , Option ['p']     ["descriptor"]
         (ReqArg (\ f opts -> opts { yateProjectDescriptor =  f }) "FILE")
         "project descriptor FILE"
     ]

parseOptions :: [String] -> IO (YateConfig , [String])
parseOptions args = case getOpt Permute options args of
    (opts,rest,[])   -> return (foldl (flip id) defaultConfig opts, rest)
    (_   ,_   ,errs) -> fail $ concat errs  ++ usageInfo headline options


detailedUsage :: String
detailedUsage = usageInfo headline options ++ "\n" ++ usage

main :: IO ()
main = do
  (config, parsed) <- getArgs >>= parseOptions
  if (yateHelp config) then
    putStrLn $ detailedUsage
  else do
    description <- case parsed of
          []     -> readDescriptor (yateProjectDescriptor config)
          desc:_ -> readDescriptor desc
    case description of 
      Right d -> runTemplate config d
      Left e  -> putStrLn ("Error running template: "++ e) >> putStrLn detailedUsage

runTemplate :: YateConfig -> ProjectDescription -> IO ()
runTemplate config d = do
  created <- makeTemplateInstance (yateProjectType config) (yateOutputDirectory config) d
  putStrLn "Copied Files: "
  putStrLn $ concat $ intersperse "\n" (map ("\t" ++) created)

readDescriptor :: FilePath -> IO (Either String ProjectDescription)
readDescriptor cf = do
  b <- doesFileExist cf 
  if b then
    readFile cf >>= return . Right . readDescription
  else
    return $ Left ("project descriptor '" ++ cf ++ "' is not readable")
