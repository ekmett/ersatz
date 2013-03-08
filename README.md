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
verify_currying :: SAT ()
verify_currying = forall $ \x y z ->
  assert $ ((x && y) ==> z) === (x ==> y ==> z)
```


Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett

