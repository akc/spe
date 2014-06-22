# The `spe` package &mdash; species lite

A simple library for combinatorial species. For more information see the
[reference documentation](http://hackage.haskell.org/package/spe)
on Hackage.

If you want something more substantial, then you will most likely be
happier with the
[`species` package](http://hackage.haskell.org/package/species)
by Brent Yorgey.

## Examples

### Octopodes

An octopus is a cycle of nonempty lists:
```haskell
oct = cyc `o` nonempty list
```

### Binary trees

```haskell
data BTree a = Empty | BNode a (BTree a) (BTree a) deriving (Show, Eq)

-- The species of binary trees
btree :: Spe a (BTree a)
btree [] = [ Empty ]
btree xs = [ BNode v l r
           | (v,(l,r)) <- x .*. (btree .*. btree) $ xs
           ]
```

### A combinatorial equality

The following expression evaluates to true and illustrates that the
derivative of the species of cycles is isomorphic to the species of
lists (linear orders):
```haskell
(map catMaybes $ diff cyc [1..5]) == list [1..5]
```
