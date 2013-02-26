module Bio.Phylo where


import qualified Bio.Phylo.Tree as Tree
import qualified Bio.Phylo.IO.Newick as Newick


parse :: String -> (String -> Tree.Tree)
parse "newick" = Newick.parse
parse_file :: String -> (String -> IO Tree.Tree)
parse_file "newick" = Newick.parse_file

write :: String -> (Tree.Tree -> String)
write "newick" = Newick.write
write_file :: String -> (String -> Tree.Tree -> IO ())
write_file "newick" = Newick.write_file

convert :: String -> String -> String -> String -> IO ()
convert from_file from_format to_file to_format =
    do tree <- parse_file from_format from_file
       write_file to_format to_file tree


-- default Show instance for tree and clade
instance Show(Tree.Tree) where
    show tree = Newick.write tree
instance Show(Tree.Clade) where
    show clade = Newick.write_clade clade
    
    
-- get a list of terminal nodes from a tree
terminals :: Tree.Tree -> [Tree.Clade]
terminals (Tree.RootedTree root) = clade_terminals root
clade_terminals :: Tree.Clade -> [Tree.Clade]
clade_terminals clade = case Tree.children clade of
                          [] -> [clade]
                          otherwise -> [child | direct_child <- otherwise, 
                                                child <- clade_terminals (direct_child)]
-- find all nodes with a given label
find_nodes :: Tree.Tree -> String -> [Tree.Clade]
find_nodes (Tree.RootedTree root) string = clade_find_nodes root string
clade_find_nodes :: Tree.Clade -> String -> [Tree.Clade]
clade_find_nodes clade string = if Tree.name clade == string
                                then clade : result
                                else result
                                where result = [result | child <- Tree.children clade, result <- clade_find_nodes child string]

{-
-- get the path to the root for a clade in a tree
get_path :: Tree -> Clade -> [Clade]
get_path tree clade = TODO
get_path_to_root :: Tree -> Clade -> [Clade]
get_path_to_root (RootedTree root) clade = TODO

-- get the MRCA for a set of clades
mrca :: Tree -> [Clade] -> Clade
mrca (RootedTree root) [] = root
mrca tree h:[] = h
mrca tree h:t = mrca_2 tree h (mrca tree t:tail(t))
mrca_2 (RootedTree root) clade1 clade2 = 
    [ancestor | ancestor <- ancestors1, contains [ancestor] ancestors2] !! 0
    where ancestors1 = get_path_to_root clade1
          ancestors2 = get_path_to_root clade2

-- get the sum of the branch lengths between two nodes
distance tree clade1 clade2 = 
    (distance_to_descendant ancestor clade1) + (distance_to_descendant ancestor clade2)
    where ancestor = mrca tree [clade1, clade2]
distance_to_descendant ancestor descendant = TODO -}
