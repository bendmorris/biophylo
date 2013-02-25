module Bio.Phylo.BaseTree where

import Data.List.Utils


data Tree = EmptyTree | RootedTree Clade
    deriving (Eq)
data Clade = Node {
                   name :: String,
                   branch_length :: Float,
                   comment :: String,
                   children :: [Clade]
                   }
    deriving (Eq)
                      
terminals :: Tree -> [Clade]
terminals tree = case tree of
                   EmptyTree -> []
                   RootedTree root -> clade_terminals root
                   
clade_terminals :: Clade -> [Clade]
clade_terminals clade = case children clade of
                          [] -> [clade]
                          otherwise -> [child | direct_child <- otherwise, 
                                                child <- clade_terminals (direct_child)]
