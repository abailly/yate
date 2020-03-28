{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Handles template instantiation and project descriptors.
module Yate.Template(doTemplate,
                     readDescription,
                     ProjectDescription) where

import Control.Applicative ((<|>))
import Control.Monad (mplus)
import Data.Aeson (FromJSON(..), Value(Object, String), eitherDecode, withObject)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as H
import qualified Data.Text as Text
import Text.Regex.Base.RegexLike
import Text.Regex.PCRE (compDotAll, (=~))

-- | A single "node"
data Node a = S a
              -- ^A leaf node
            | L [ Tree a ]
              -- ^A intemediate node leading to children `Tree`s
  deriving (Eq, Show, Read)

instance FromJSON (Node String) where
  parseJSON o@(Object obj) =
    L <$> mapM toTree (H.toList obj)
    where
      toTree (k, v) = (Text.unpack k :>:) <$> parseJSON v

  parseJSON (String s) = pure $ S (Text.unpack s)
  parseJSON other = fail $ "cannot parse Node from " <> show other

-- | A tree whose leaves are key/values pairs and nodes are labelled n-way trees.
data Tree a =  String :>: Node a
  deriving (Eq, Show, Read)

instance FromJSON (Tree String) where
  parseJSON =
    withObject "Tree" $
    \ obj -> case H.toList obj of
               [(k, v)] -> (Text.unpack k :>:) <$> parseJSON v
               other -> fail $ "cannot parse Tree from " <> show other

newtype ProjectDescription = Project { projectTree :: Tree String }
  deriving (Eq)

instance Show ProjectDescription where
  show (Project tree) = show tree

instance Read ProjectDescription where
  readsPrec n s =
    case readsPrec n s of
      [(tree,"")] -> [(Project tree, "")]
      other -> fail $ "cannot read ProjectDescription from " <> show other

instance FromJSON ProjectDescription where
  parseJSON v = Project <$> parseJSON v


-- | Read a project description in JSON format
--
-- Input JSON must be an object with a single root key and then a recursive
-- structure of objects and strings.
--
-- >>> :set -XOverloadedStrings
-- >>> readJSONDescription "{\"project\": { \"name\": \"myproj\"}}"
-- Right "project" :>: L ["name" :>: S "myproj"]
--
-- >>> readJSONDescription "{\"project\": { \"name\": [ \"myproj\" ]}}"
-- Left "Error in $: cannot parse Node from Array [String \"myproj\"]"
--
-- >>> readJSONDescription "{\"project\": \"foo\", \"bar\" : { \"name\": [ \"myproj\" ]}}"
-- Left "Error in $: cannot parse Tree from [(\"project\",String \"foo\"),(\"bar\",Object (fromList [(\"name\",Array [String \"myproj\"])]))]"
readJSONDescription ::
  LBS.ByteString -> Either String ProjectDescription
readJSONDescription = eitherDecode

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
-- >>> instantiate (Project ("foo" :>: L ["qix" :>: S "foo", "bar" :>: S "baz" ]))  "foo"
-- "foo"
-- >>> instantiate (Project ("foo" :>: L ["qix" :>: S "foo", "bar" :>: S "baz" ]))  "{{foo.qix}}"
-- "foo"
-- >>> instantiate (Project ("foo" :>: L ["qix" :>: S "foo", "bar" :>: S "baz" ]))  "baz{{foo.qix}}bar"
-- "bazfoobar"
-- >>> instantiate (Project ("foo" :>: L ["qix" :>: S "foo", "bar" :>: S "baz" ]))  "baz{{foo.qix}}bar{{foo.bar}}"
-- "bazfoobarbaz"
-- >>> instantiate (Project ("foo" :>: L ["qix" :>: S "foo", "bar" :>: S "baz" ]))  "baz{{foo}}bar{{foo.bar}}"
-- "bazbarbaz"
instantiate :: ProjectDescription -> String -> String
instantiate project input =
  let (beg,found,end,subs) = input =~ "{{([^}]*)}}" :: (String, String, String, [String])
  in  case found of
        "" -> input
        _  -> case select (path (head subs)) (projectTree project) of
                Just v  -> beg ++ v ++ instantiate project end
                Nothing -> beg ++      instantiate project end

-- |Instantiate list templates, eg. templates with multiple values
--
-- >>> instantiateMult (Project ("foo" :>: L ["name" :>: S "foo", "name" :>: S "baz" ])) "{{foo.name}}{{#foo}}name: {{name}}\n{{/foo}}"
-- "fooname: foo\nname: baz\n"
-- >>> instantiateMult (Project ("project" :>: L ["name" :>: S "foo", "authors" :>: L ["author" :>: S "baz" ]])) "{{#project.authors}}name: {{author}}\n{{/project.authors}}"
-- "name: baz\n"
instantiateMult :: ProjectDescription -> String -> String
instantiateMult project input =
  let regex = makeRegexOpts (defaultCompOpt + compDotAll) defaultExecOpt "{{#([^}]*)}}(.*){{/\\1}}"
      (beg,found,end,subs) = match regex input :: (String, String, String, [String])
  in case found of
       "" -> input
       _  -> case selectMult (path (head subs)) (projectTree project) of
               []   -> instantiate project beg ++ instantiate project end
               rest -> instantiate project beg
                       ++ concatMap (flip instantiate (head $ tail subs) . Project) rest
                       ++ instantiateMult project end

-- |Main template function
--
-- Instantiate variables and blocks references found in @input@ with the contnet of @descriptor@
doTemplate :: ProjectDescription  -- ^Context for template instantiation
            -> String             -- ^Input string
            -> String             -- ^Output string
doTemplate descriptor =
  instantiate descriptor . instantiateMult descriptor
