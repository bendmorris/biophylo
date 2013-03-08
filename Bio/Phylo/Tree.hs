module Bio.Phylo.Tree where

import qualified Data.ByteString.Lazy as B


data Tree = Tree Clade
    deriving (Eq)
data Clade = Clade {
                    name :: B.ByteString,
                    branch_length :: Float,
                    comment :: B.ByteString,
                    children :: [Clade]
                    }
    deriving (Eq)

emptyClade = Clade { 
    name = B.empty, 
    branch_length = 1, 
    comment = B.empty, 
    children = [] 
}
