{-
biophylo IO module providing support for the Newick tree format.
-}
module Bio.Phylo.IO.Newick where

import qualified Bio.Phylo.BaseTree as BaseTree
import Text.Regex.PCRE
import Data.List.Utils


data TokenName = OpenParens 
               | CloseParens 
               | UnquotedNodeLabel 
               | EdgeLength
               | Comma
               | Comment
               | QuotedNodeLabel
               | Semicolon
               | Newline
               | Error
               deriving (Show, Eq)
type Token = (String, TokenName)

{-  
    List of regex matches and their respective TokenNames.

    When a regex contains multiple match groups, it needs to be repeated with a 
    blank string below for matching to work correctly.
-}
tokens :: [(String, TokenName)]
tokens = [
          ("\\(",                               OpenParens),
          ("\\)",                               CloseParens),
          ("[^\\s\\(\\)\\[\\]\\'\\:\\;\\,]+",   UnquotedNodeLabel),
          ("\\:[0-9]*\\.?[0-9]+",               EdgeLength),
          ("\\,",                               Comma),
          ("\\[(\\\\.|[^\\]])*\\]",             Comment),
          ("",                                  Comment),
          ("\\'(\\\\.|[^\\'])*\\'",             QuotedNodeLabel),
          ("",                                  QuotedNodeLabel),
          ("\\;",                               Semicolon),
          ("\\n",                               Newline),
          (".*",                                Error)
          ]
          
token_regex = [fst token | token <- tokens, fst token /= ""]
token_names = [snd token | token <- tokens]

newick_regex = join "|" ["(" ++ regex ++ ")" | regex <- token_regex]



-- Parser

parse :: String -> BaseTree.Tree
parse string = if length result == 1 
               then BaseTree.RootedTree $ result !! 0
               else BaseTree.RootedTree $ (fst $ make_clade [] "" 1 "" result) !! 0
               where result = process_tokens $ tokenize string
                        
tokenize :: String -> [Token]
tokenize string = [
                   (head token, 
                    [fst pair 
                     | pair <- zip token_names (tail token), 
                       (snd pair) /= ""] !! 0)
                   | token <- string =~ newick_regex :: [[String]],
                   head token /= ""
                   ]
                   
-- process tokens into a list of clades
process_tokens :: [Token] -> [BaseTree.Clade]
process_tokens [] = []
process_tokens tokens = fst (new_clade tokens)
new_clade tokens = make_clade tokens "" 1 "" []
trim string = [string !! n | n <- [1 .. (length(string) - 2)]]
make_clade :: [Token] -> String -> Float -> String -> [BaseTree.Clade] -> ([BaseTree.Clade], [Token])
make_clade (h:t) name branch_length comment children = 
    case snd h of
      OpenParens -> make_clade (snd result) name branch_length comment (fst result)
                    where result = new_clade t
      CloseParens -> (fst $ make_clade [] name branch_length comment children, t)
      UnquotedNodeLabel -> make_clade t (fst h) branch_length comment children
      EdgeLength -> make_clade t name ((read $ tail $ fst h) :: Float) comment children
      Comma -> (fst (make_clade [] name branch_length comment children) ++ (fst result), snd result)
               where result = new_clade t
      Comment -> make_clade t name branch_length (trim $ fst h) children
      QuotedNodeLabel -> make_clade ((trim $ fst h, UnquotedNodeLabel):t) 
                                        name branch_length comment children
      Semicolon -> make_clade [] name branch_length comment children
      Newline -> make_clade t name branch_length comment children
      Error -> ([], [])
make_clade [] name branch_length comment children = 
    ([BaseTree.Node { 
        BaseTree.name = name,
        BaseTree.branch_length = branch_length,
        BaseTree.comment = comment,
        BaseTree.children = children }],
     [])
     
     

-- Writer

write :: BaseTree.Tree -> String
write tree = case tree of
               BaseTree.EmptyTree -> "();"
               BaseTree.RootedTree clade -> write_clade clade ++ ";"
write_clade :: BaseTree.Clade -> String
write_clade clade = (if BaseTree.children clade == [] then "" 
                     else ("(" ++ (join "," [write_clade child | child <- BaseTree.children clade]) ++ ")")) ++
                    (case BaseTree.name clade of
                       "" -> ""
                       label -> case match of
                                  ("", _, "") -> label
                                  otherwise -> "'" ++ label ++ "'"
                                where match = label =~ (token_regex !! 2) :: (String, String, String)
                    ) ++
                    (if BaseTree.branch_length clade == 1.0
                     then ""
                     else ":" ++ (show $ BaseTree.branch_length clade)) ++
                    (if BaseTree.comment clade == "" then "" else "[" ++ BaseTree.comment clade ++ "]")
