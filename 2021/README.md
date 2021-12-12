# Haskell for 2021

To compile `hello.hs` use `ghc -o hello hello.hs`. Where `ghc` is the Glasgow Haskell Compiler.

For more complicated files you can `ghc -O2 --make 000-getting-started-3.hs -threaded -rtsopts`. Exactly why? Not sure yet.

For very simple files `ghc 001-part1.hs` works too (i.e. not specifying `-o`).
