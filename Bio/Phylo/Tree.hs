module Bio.Phylo.Tree where

import qualified Data.ByteString.Lazy as B


data Tree = RootedTree Clade
    deriving (Eq)
data Clade = Node {
                   name :: B.ByteString,
                   branch_length :: Float,
                   comment :: B.ByteString,
                   children :: [Clade]
                   }
    deriving (Eq)
