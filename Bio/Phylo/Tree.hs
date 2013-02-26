module Bio.Phylo.Tree where


data Tree = RootedTree Clade
    deriving (Eq)
data Clade = Node {
                   name :: String,
                   branch_length :: Float,
                   comment :: String,
                   children :: [Clade]
                   }
    deriving (Eq)
