module Bio.Phylo.Tree where

import Data.List.Utils


data Tree = RootedTree Clade
    deriving (Eq)
data Clade = Node {
                   name :: String,
                   branch_length :: Float,
                   comment :: String,
                   children :: [Clade]
                   }
    deriving (Eq)
