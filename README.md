# The `spe` package &mdash; species lite

A simple library for combinatorial species with no dependencies but base.
For a quick taste look at the examples below. For more information see the
[reference documentation](http://hackage.haskell.org/package/spe)
on Hackage, and the full (but short!) source code:
[Math/Spe.hs](https://github.com/akc/spe/blob/master/Math/Spe.hs).

If you want something more substantial, then you will most likely be
happier with the excellent
[`species` package](http://hackage.haskell.org/package/species)
by Brent Yorgey.

## Examples

### Octopodes

An octopus is a cycle of nonempty lists:
```haskell
oct = cyc `o` nonEmpty list
```

### Connected lists

A connected list is a nonempty list that begins with its smallest
element. E.g, `[3,5,9,7]` is connected but `[2,4,1]` is not. Using the
ordinal product we can define the L-species of connected lists by
```haskell
listc = x <*. list
```
in which `x` is the singleton species. Can you explain why the species
`list` and `` set `o` listc `` are isomorphic?

### Binary trees

Here's an example of a recursively defined species.

```haskell
data BTree a = Empty | BNode a (BTree a) (BTree a) deriving (Show, Eq)

btree :: Spe a (BTree a)
btree [] = [ Empty ]
btree xs = [ BNode v l r
           | (v,(l,r)) <- x .*. (btree .*. btree) $ xs
           ]
```

### Derivatives

The following expression evaluates to true and illustrates that the
derivative of the species of cycles is isomorphic to the species of
lists (linear orders):
```haskell
(map catMaybes $ diff cyc [1..5]) == list [1..5]
```
