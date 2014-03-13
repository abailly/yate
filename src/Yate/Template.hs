-- | Handles template instantiation and project descriptors.
module Yate.Template(doTemplate,
                     readDescription,
                     ProjectDescription) where
import Text.Regex.PCRE((=~),compDotAll)
import Text.Regex.Base.RegexLike
import Control.Monad(mplus)

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
                                      
-- |Main template function
--
-- Instantiate variables and blocks references found in @input@ with the contnet of @descriptor@
doTemplate :: ProjectDescription  -- ^Context for template instantiation
            -> String             -- ^Input string
            -> String             -- ^Output string
doTemplate descriptor = instantiate descriptor . instantiateMult descriptor
