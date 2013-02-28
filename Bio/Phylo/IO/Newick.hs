{-
biophylo IO module providing support for the Newick tree format.
-}
module Bio.Phylo.IO.Newick where

import qualified Bio.Phylo.Tree as Tree
import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString.Lazy
import Data.List
import qualified Data.ByteString.Lazy.Char8 as B


data TokenName = OpenParens 
               | CloseParens 
               | UnquotedNodeLabel 
               | EdgeLength
               | Comma
               | Comment
               | QuotedNodeLabel
               deriving (Show, Eq)
type Token = (B.ByteString, TokenName)

join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)

{-  
    List of regex matches and their respective TokenNames.

    When a regex contains multiple match groups, it needs to be repeated with a 
    blank string below for matching to work correctly.
-}
tokens :: [Token]
tokens = [(B.pack(fst token), snd token)
          | token <- [
          ("\\(",                               OpenParens),
          ("\\)",                               CloseParens),
          ("[^][[:space:]()':;,]+",             UnquotedNodeLabel),
          ("[:][0-9]*[.]?[0-9]+",               EdgeLength),
          ("\\,",                               Comma),
          ("[[](\\\\.|[^]])*[]]",               Comment),
          ("",                                  Comment),
          ("'(\\\\.|[^'])*'",                   QuotedNodeLabel),
          ("",                                  QuotedNodeLabel)
          ]]
          
token_regex = [fst token | token <- tokens, fst token /= B.empty]
token_names = [snd token | token <- tokens]

newick_regex = B.intercalate (B.pack "|") [B.concat [B.pack "(", regex, B.pack ")"] | regex <- token_regex]



-- Parser

parse :: B.ByteString -> Tree.Tree
parse string = if length result == 1 
               then Tree.Tree $ result !! 0
               else Tree.Tree $ (fst $ make_clade [] B.empty 1 B.empty result) !! 0
               where result = process_tokens $ tokenize string
                        
tokenize :: B.ByteString -> [Token]
tokenize string = [
                   (head token,
                    [fst pair 
                     | pair <- zip token_names (tail token), 
                       (snd pair) /= B.empty] !! 0)
                   | token <- string =~ newick_regex :: [[B.ByteString]],
                   head token /= B.empty
                   ]
                   
-- process tokens into a list of clades
process_tokens :: [Token] -> [Tree.Clade]
process_tokens [] = []
process_tokens tokens = fst (new_clade tokens)
new_clade tokens = make_clade tokens B.empty 1 B.empty []
trim string = B.take ((B.length string) - 2) $ B.drop 1 string
-- make_clade takes a list of tokens generates a set of clades with the same parent,
-- and returns those clades and the remaining unparsed tokens
make_clade :: [Token] -> B.ByteString -> Float -> B.ByteString -> [Tree.Clade] -> ([Tree.Clade], [Token])
make_clade (h:t) name branch_length comment children = 
    case snd h of
      OpenParens -> make_clade (snd result) name branch_length comment (fst result)
                    where result = new_clade t
      CloseParens -> (fst $ make_clade [] name branch_length comment children, t)
      UnquotedNodeLabel -> make_clade t (B.concat [name, fst h]) branch_length comment children
      EdgeLength -> make_clade t name ((read $ B.unpack $ B.tail $ fst h) :: Float) comment children
      Comma -> (fst (make_clade [] name branch_length comment children) ++ (fst result), snd result)
               where result = new_clade t
      Comment -> make_clade t name branch_length (B.concat [comment, trim $ fst h]) children
      QuotedNodeLabel -> make_clade ((trim $ fst h, UnquotedNodeLabel):t) 
                                        name branch_length comment children
make_clade [] name branch_length comment children = 
    ([Tree.Clade { 
        Tree.name = name,
        Tree.branch_length = branch_length,
        Tree.comment = comment,
        Tree.children = children }],
     [])
     
     

-- Writer

write :: Tree.Tree -> B.ByteString
write (Tree.Tree root) = B.concat [write_clade root, B.pack ";\n"]

write_clade :: Tree.Clade -> B.ByteString
write_clade clade = B.concat [
                    (if Tree.children clade == [] then B.empty 
                     else B.concat [B.pack "(", (B.intercalate (B.pack ",") [write_clade child | child <- Tree.children clade]), B.pack ")"]),
                    (if label == B.empty then B.empty
                     else (if match then label else B.concat [B.pack "'", label, B.pack "'"])
                    ),
                    (if Tree.branch_length clade == 1.0
                     then B.empty
                     else B.concat [B.pack ":", B.pack $ show $ Tree.branch_length clade]),
                    (if Tree.comment clade == B.empty then B.empty else B.concat [B.pack "[", Tree.comment clade, B.pack "]"])
                    ]
                     where label = Tree.name clade
                           match = label =~ (B.concat [B.pack "^", (token_regex !! 2), B.pack "$"]) :: Bool
