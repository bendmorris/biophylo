module Bio.Phylo where


import qualified Bio.Phylo.BaseTree as BaseTree
import qualified Bio.Phylo.IO.Newick as Newick
import Data.Char


parse :: String -> (String -> BaseTree.Tree)
parse "newick" = Newick.parse
parse_file :: String -> (String -> IO BaseTree.Tree)
parse_file "newick" = Newick.parse_file

write :: String -> (BaseTree.Tree -> String)
write "newick" = Newick.write
write_file :: String -> (BaseTree.Tree -> String -> IO ())
write_file "newick" = Newick.write_file


-- default Show instance for tree and clade
instance Show(BaseTree.Tree) where
    show tree = Newick.write tree
instance Show(BaseTree.Clade) where
    show clade = Newick.write_clade clade
