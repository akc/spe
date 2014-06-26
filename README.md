# The `spe` package &mdash; species lite

A simple library for combinatorial species with no dependencies but base.
For a quick taste, look at the examples below. For further information, see the
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

### Ballot matrices

A ballot (or ordered set partition) is a list of blocks, where a block
is simply a nonempty set. So may give it this type
```haskell
type Bal a = [[a]]
```

The ballot species can be defined by ``list `o` nonEmpty set``. The type
of this expression is ``Spe a ([[a]],[[a]])`` and it doesn't, however,
directly fit the type we intended. Looking at the definition of
partitional composition we realize that ``mapM (nonEmpty set) bs == bs``
for any set partition `bs`. Thus the second component is redundant, and
a better definition of the species of ballots would be

```haskell
bal :: Spe a (Bal a)
bal = map fst . (list `o` nonEmpty set)
```

The species of ballots is already defined in
[Math/Spe.hs](https://github.com/akc/spe/blob/master/Math/Spe.hs).
The definition given there is a bit different for reasons of efficiency.

In a recent [preprint](http://arxiv.org/abs/1405.2441)
[Stuart Hannah](https://personal.cis.strath.ac.uk/stuart.a.hannah/) and I
study upper-triangular matrices of ballots without empty rows.
Those can be defined as follows:

```haskell
type Row a = [Bal a]
type BalMat a = [Row a]

balAlt :: Spe a (Bal a)
balAlt = map fst . (list `o` nonEmpty set)

rowOfLength :: Int -> Spe a (Row a)
rowOfLength i = bal .^ i

balMatOfDim :: Int -> Spe a (BalMat a)
balMatOfDim k = prod [ nonEmpty (rowOfLength i) | i <- [1..k] ]

balMat :: Spe a (BalMat a)
balMat xs = assemble [ balMatOfDim k | k <- [0..length xs] ] xs
```

Further, define the sign of a ballot matrices as:

```haskell
sign :: BalMat a -> Int
sign m = (-1)^(dim m + blk m)
  where
    dim = length                   -- Matrix dimension
    blk = length . concat . concat -- Total number of blocks
```

Let us now count ballot matrices with respect to this sign:

```
> [ sum . map sign $ balMat [1..n] | n<-[0..6] ]
[1,1,3,19,207,3451,81663]
```

Looking up this sequence in [OEIS](http://oeis.org/), here using the
command line utility [`sloane`](https://github.com/akc/sloane), we find:

```
$ sloane 1,1,3,19,207,3451,81663

S A079144 1,1,3,19,207,3451,81663,2602699,107477247,5581680571,356046745023,
N A079144 Number of labeled interval orders on n elements: (2+2)-free posets.
```

In the aforementioned preprint we prove this surprising result using a
sign reversing involution.
