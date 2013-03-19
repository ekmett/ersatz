0.2.2
-----
* Added examples to the documentation.
* Made the examples build as `ersatz-sudoku` and `ersatz-regexp-grid`.

0.2.1
-----
* Added `examples/sudoku`, a sudoku solver.

0.2.0.1
-------
* Fixed an overly conservative bound on `containers`.

0.2
---
* Converted to `Control.Lens` internally.
* Added `Ersatz.Solver.DepQBF`
* Added a bunch of example dimacs files
* The types now prevent one from applying a solver that does not support QSAT
  to a problem that requires it
* Added `examples/regexp-grid`, a program that solves the [regular expression
  crossword](http://www.coinheist.com/rubik/a_regular_crossword/grid.pdf)
* Made some optimizations to the formula generation. `regexp-grid` went from
  71737 literals and 427725 clauses to 8618 literals and 172100 clauses and got
  much faster
* Based `and` and `or` in `Boolean` on `Foldable`; added `all` and `any`

0.1.0.2
-----
* Added correct links to the source repository and issue tracker to the cabal project

0.1
---
* Repository Initialized
