module Bio.Phylo where

import qualified Data.ByteString.Lazy.Char8 as B
import Bio.Phylo.Tree
import qualified Bio.Phylo.IO.Newick as Newick
--import qualified Bio.Phylo.IO.Nexus as Nexus


parse :: String -> (B.ByteString -> Tree)
parse "newick" = Newick.parse
--parse "nexus" = Nexus.parse

write :: String -> (Tree -> B.ByteString)
write "newick" = Newick.write
--write "nexus" = Nexus.write

parse_file :: String -> String -> IO Tree
parse_file format filename =
    do file_contents <- B.readFile filename
       return $ parse format $ file_contents

write_file :: String -> String -> Tree -> IO ()
write_file format filename tree = 
    B.writeFile filename $ write format $ tree

convert :: String -> String -> String -> String -> IO ()
convert from_file from_format to_file to_format =
    do tree <- parse_file from_format from_file
       write_file to_format to_file tree


-- default Show instance for tree and clade
instance Show(Tree) where
    show tree = B.unpack $ Newick.write tree
instance Show(Clade) where
    show clade = B.unpack $ Newick.write_clade clade
    
    
-- get a list of terminal nodes from a tree
terminals :: Tree -> [Clade]
terminals (Tree root) = clade_terminals root
clade_terminals :: Clade -> [Clade]
clade_terminals clade = case children clade of
                          [] -> [clade]
                          otherwise -> [child | direct_child <- otherwise, 
                                                child <- clade_terminals (direct_child)]
-- find all nodes with a given label
find_nodes :: Tree -> B.ByteString -> [Clade]
find_nodes (Tree root) string = clade_find_nodes root string
clade_find_nodes :: Clade -> B.ByteString -> [Clade]
clade_find_nodes clade string = if name clade == string
                                then clade : result
                                else result
                                where result = [result | child <- children clade, result <- clade_find_nodes child string]

{-
-- get the path to the root for a clade in a tree
get_path :: Tree -> Clade -> [Clade]
get_path tree clade = TODO
get_path_to_root :: Tree -> Clade -> [Clade]
get_path_to_root (Tree root) clade = TODO

-- get the MRCA for a set of clades
mrca :: Tree -> [Clade] -> Clade
mrca (Tree root) [] = root
mrca tree h:[] = h
mrca tree h:t = mrca_2 tree h (mrca tree t:tail(t))
mrca_2 (Tree root) clade1 clade2 = 
    [ancestor | ancestor <- ancestors1, contains [ancestor] ancestors2] !! 0
    where ancestors1 = get_path_to_root clade1
          ancestors2 = get_path_to_root clade2

-- get the sum of the branch lengths between two nodes
distance tree clade1 clade2 = 
    (distance_to_descendant ancestor clade1) + (distance_to_descendant ancestor clade2)
    where ancestor = mrca tree [clade1, clade2]
distance_to_descendant ancestor descendant = TODO -}
