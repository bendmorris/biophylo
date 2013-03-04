import Bio.Phylo

main = do -- read in a Newick file
          tree <- parse_file "newick" "test.newick"

          -- print all of the terminal nodes
          putStrLn $ show $ terminals tree

          -- write the tree to a new output file
          write_file "newick" "output.newick" tree

