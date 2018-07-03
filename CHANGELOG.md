0.4.3 [2018.07.03]
------------------
* Make the test suite compile on GHC 8.6.
* Allow building with `containers-0.6`.

0.4.2
-----
* Add `Semigroup` instances for `Clause` and `Formula`.
* Generalize `regular`, `regular_in_degree`, `regular_out_degree`,
  `max_in_degree`, `min_in_degree`, `max_out_degree`, and `min_out_degree` to
  work over heterogeneous relations.
* Add `buildFrom` to `Ersatz.Relation.Data`.
* Add `difference`, `reflexive_closure`, and `symmetric_closure` to
  `Ersatz.Relation.Op`.
* Add `anti_symmetric` and `total` to `Ersatz.Relation.Prop`.

0.4.1
-----
* Add a library dependency on the `doctests` test suite

0.4
---
* Performance improvements for CNF printing and parsing
* Add the `Ersatz.Counting`, `Ersatz.Relation`, `Ersatz.Relation.Data`,
  `Ersatz.Relation.Prop`, and `Ersatz.Relation.Op` modules
* Eliminate the `Or` constructor from `Bit` towards using AIG
* Fix error in the SAT encoding of the `choose` function
* Revamp `Setup.hs` to use `cabal-doctest`. This makes it build
  with `Cabal-2.0`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.

0.3.1
-----
* Removed the explicit `Safe` annotations. They can't be maintained by mere mortals. Patches to mark packages upstream as `Trustworthy` will be accepted as needed.

0.3
-----
* Unified `Encoding` and `Decoding` into `Codec`
* Unified the `forall` and `exists` implementations into a single `literally`
  method in `Variable` class.
* Added `Orderable` type class and instances
* Added `Ersatz.Bits.Bits` for variable-sized bit arithmetic.
* Renamed `Ersatz.Bits.half_adder` to `halfAdder`
* Renamed `Ersatz.Bits.full_adder` to `fullAdder`
* Added new examples
* Dropped `blaze` package dependency in favor of newer `bytestring`
* Significantly shrank the number of "Trustworthy" modules
* Added various generic `V1` instances
* Added `Equatable` instances for `Map` and `IntMap`
* Added `Ersatz.BitChar` module for computing with `Char` and `String`
* Wider version bounds for `transformers` and `mtl`.

0.2.6.1
-----
* Exported `Ersatz.Variable.GVariable`

0.2.6
-----
* `temporary 1.2` support

0.2.5.1
-------
* Slightly faster builds due to dropping the need for `template-haskell`.

0.2.5
-----
* Support for GHC 7.8 and `lens` 4.x

0.2.3
-----
* SafeHaskell support

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
