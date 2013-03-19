# Ersatz

[![Build Status](https://secure.travis-ci.org/ekmett/ersatz.png)](http://travis-ci.org/ekmett/ersatz)

Ersatz is a library for generating QSAT (CNF/QBF) problems using a monad. It takes care of generating the normal form, encoding your problem, marshaling the data to an external solver, and parsing and interpreting the result into Haskell types.

What differentiates Ersatz is the use of observable sharing in the API.

For instance to define a full adder:

```haskell
full_adder :: Bit -> Bit -> Bit -> (Bit, Bit)
full_adder a b cin = (s2, c1 || c2)
  where (s1,c1) = half_adder a b
        (s2,c2) = half_adder s1 cin

half_adder :: Bit -> Bit -> (Bit, Bit)
half_adder a b = (a `xor` b, a && b)
```

as opposed to the following code in [satchmo](http://dfa.imn.htwk-leipzig.de/satchmo/):


```haskell
full_adder :: Boolean -> Boolean -> Boolean
           -> SAT ( Boolean, Boolean )
full_adder a b c = do
  let s x y z = sum $ map fromEnum [x,y,z]
  r <- fun3 ( \ x y z -> odd $ s x y z ) a b c
  d <- fun3 ( \ x y z -> 1   < s x y z ) a b c
  return ( r, d )

half_adder :: Boolean -> Boolean
           -> SAT ( Boolean, Boolean )
half_adder a b = do
  let s x y = sum $ map fromEnum [x,y]
  r <- fun2 ( \ x y -> odd $ s x y ) a b
  d <- fun2 ( \ x y -> 1   < s x y ) a b
  return ( r, d )
```

This enables you to use the a much richer subset of Haskell than the purely monadic meta-language, and it becomes much easier to see that the resulting encoding is correct.

To allocate fresh existentially or universally quantified variables or to assert that a Bit is true and add the attendant circuit with sharing to the current problem you use the SAT monad.

```haskell
verify_currying :: (MonadState s m, HasQSAT s) => m ()
verify_currying = do
  (x::Bit, y::Bit, z::Bit) <- forall
  assert $ ((x && y) ==> z) === (x ==> y ==> z)
```

We can then hand that off to a SAT solver, and get back an answer:

```haskell
main = solveWith depqbf verify_currying >>= print
```

Support is offered for decoding various Haskell datatypes from the
solution provided by the SAT solver.

# Examples

Included are a couple of examples included with the distribution.
Neither are as fast as a dedicated solver for their respective
domains, but they showcase how you can solve real world problems
involving 10s or 100s of thousands of variables and constraints
with `ersatz`.

## sudoku

```
% time ersatz-sudoku
Problem:
┌───────┬───────┬───────┐
│ 5 3   │   7   │       │
│ 6     │ 1 9 5 │       │
│   9 8 │       │   6   │
├───────┼───────┼───────┤
│ 8     │   6   │     3 │
│ 4     │ 8   3 │     1 │
│ 7     │   2   │     6 │
├───────┼───────┼───────┤
│   6   │       │ 2 8   │
│       │ 4 1 9 │     5 │
│       │   8   │   7 9 │
└───────┴───────┴───────┘
Solution:
┌───────┬───────┬───────┐
│ 5 3 4 │ 6 7 8 │ 9 1 2 │
│ 6 7 2 │ 1 9 5 │ 3 4 8 │
│ 1 9 8 │ 3 4 2 │ 5 6 7 │
├───────┼───────┼───────┤
│ 8 5 9 │ 7 6 1 │ 4 2 3 │
│ 4 2 6 │ 8 5 3 │ 7 9 1 │
│ 7 1 3 │ 9 2 4 │ 8 5 6 │
├───────┼───────┼───────┤
│ 9 6 1 │ 5 3 7 │ 2 8 4 │
│ 2 8 7 │ 4 1 9 │ 6 3 5 │
│ 3 4 5 │ 2 8 6 │ 1 7 9 │
└───────┴───────┴───────┘
ersatz-sudoku  1,13s user 0,04s system 99% cpu 1,179 total
```

## regexp-grid

This solves the [regular expression crossword puzzle](http://www.coinheist.com/rubik/a_regular_crossword/grid.pdf) from the MIT mystery hunt.

> % time ersatz-regexp-grid

[SPOILER](notes/SPOILER.html)

> ersatz-regexp-grid  2,45s user 0,05s system 99% cpu 2,502 total

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett

