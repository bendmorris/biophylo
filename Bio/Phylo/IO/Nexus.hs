{-
biophylo IO module providing support for the NEXUS tree format.
-}
module Bio.Phylo.IO.Nexus where

import Bio.Phylo.Tree
import Bio.Phylo.IO.Newick as Newick
import Text.Parsec.ByteString.Lazy
import Text.Parsec.Expr
import Text.Parsec.Char
import Text.Parsec.Language
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Data.Char
import qualified Text.Parsec.Token as Token
import qualified Data.ByteString.Lazy.Char8 as B


data Block = Block 
    { 
     block_name :: B.ByteString, 
     contents :: [B.ByteString]
     }
     deriving (Show)


-- Parser

parse :: B.ByteString -> Tree
parse string = 
  read_trees_from_blocks (
  case Text.Parsec.Prim.parse parser "" (B.unpack string) of
    Left l -> []
    Right r -> r)

read_trees_from_blocks :: [Block] -> Tree
read_trees_from_blocks [] = Tree emptyClade
read_trees_from_blocks (h:t) =
  if (map toLower $ B.unpack $ block_name h) == "trees"
  then Newick.parse $ B.pack $ show $ contents h
  else read_trees_from_blocks t

parser = many1 block

block_lines a = 
  try (
  do end_block
     return a
  ) <|>
  do chars <- whiteSpace >> (many1 $ noneOf ";")
     whiteSpace >> symbol ";"
     block_lines (a ++ [chars])
     
end_block =
  do reserved "END"
     symbol ";"
     return ""

block = 
  do reserved "BLOCK"
     blockname <- identifier
     symbol ";"
     -- we only need to keep the lines if this is the trees block
     lines <- block_lines []
     return $ Block { block_name = B.pack blockname, contents = [B.pack line | line <- lines] }

languageDef =
  emptyDef {
            Token.commentStart    = "[",
            Token.commentEnd      = "]",
            Token.reservedNames   = [
                                     "block", "end", "trees", "tree"
                                     ],
            Token.caseSensitive   = False
            }
lexer = Token.makeTokenParser languageDef

identifier = Token.identifier       lexer
symbol     = Token.symbol           lexer
reserved   = Token.reserved         lexer
whiteSpace = Token.whiteSpace       lexer
comment    = Token.commentLine      languageDef



-- Writer

write :: Tree -> B.ByteString
write tree = B.concat [
    B.pack "#NEXUS\nBegin Trees;\n  Tree tree1=",
    Newick.write tree, 
    B.pack "\nEnd;\n"]
