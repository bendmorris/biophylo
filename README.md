This is a basic Haskell phylogenetics library, inspired by Bio::Phylo (for perl) 
and Bio.Phylo (for Python) by Rutger Vos and Eric Talevich respectively.
Lazy evaluation and recursion make functional languages like Haskell an ideal
environment to work with phylogenies.

Example usage:

    import Bio.Phylo

    main = do -- read in a Newick file
              tree <- parse_file "newick" "test.newick"

              -- print all of the terminal nodes
              putStrLn $ show $ terminals tree

              -- write the tree to a new output file
              write_file "newick" "output.newick" tree