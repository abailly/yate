{-# LANGUAGE GADTs #-}
module Main where
import System.Environment(getArgs)

type ProjectType = String

-- A node
data Node a = S String
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
   
workToDo :: ProjectType           -- ^type of project, must resolve to source template
            -> FilePath           -- ^output directory of new project
            -> ProjectDescription -- ^project description
            -> IO ()
workToDo projectType outputDirectory projectDescription =
  putStrLn $  unlines [ "Instantiating template from",
                        "\tdescription: " ++ show projectDescription,
                        "\ttype: " ++  projectType,
                        "\toutput: " ++ outputDirectory]

main :: IO ()
main = do
  [ projectType, outputDirectory, projectName ] <- getArgs
  let description = readDescription projectName
  workToDo projectType outputDirectory description
