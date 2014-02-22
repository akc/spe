-- |
-- Copyright   : Anders Claesson 2014
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
-- Species lite

module Math.Spe
    (
    -- * The species type synonym
      Spe
    -- * Constructions
    , add, assemble, mul, mulL, prod, prodL, power, powerL
    , compose, o, kDiff, diff
    -- * Specific species
    , set, one, x, ofSize, nonEmpty, kBal, bal, par, kList, list
    , cyc, perm, kSubsets, subsets
    ) where

import Data.List

-- | A species is a functor. We approximate this by a function as defined.
type Spe a c = [a] -> [c]

type Splitter a = [a] -> [([a], [a])]

decompose :: Splitter a -> Int -> [a] -> [[[a]]]
decompose _ 0 [] = [[]]
decompose _ 0 _  = []
decompose h k xs = [ b:bs | (b,ys) <- h xs, bs <- decompose h (k-1) ys ]

-- A splitter for L-species
splitL :: Splitter a
splitL [] = [([], [])]
splitL xs@(x:xt) = ([], xs) : [ (x:as, bs) | (as, bs) <- splitL xt ]

-- A splitter for B-species
splitB :: Splitter a
splitB [] = [([], [])]
splitB (x:xs) = splitB xs >>= \(ys, zs) -> [(x:ys, zs), (ys, x:zs)]

-- | Species addition
add :: Spe a b -> Spe a c -> Spe a (Either b c)
add f g xs = map Left (f xs) ++ map Right (g xs)

-- | The sum of a list of species of the same type
assemble :: [Spe a c] -> Spe a c
assemble fs xs = fs >>= \f -> f xs

-- Species multiplication
genericMul :: Splitter a -> Spe a b -> Spe a c -> Spe a (b,c)
genericMul h f g xs = [ (y, z) | (ys, zs) <- h xs, y <- f ys, z <- g zs ]

-- | Species multiplication
mul = genericMul splitB

-- | Ordinal L-species multiplication
mulL = genericMul splitL

genericProd :: Splitter a -> [Spe a b] -> Spe a [b]
genericProd h fs xs =
    let n = length fs
    in [ zipWith ($) fs bs | bs <- decompose h n xs ] >>= sequence

-- | The product of a list of species
prod = genericProd splitB

-- | The ordinal product of a list of L-species
prodL = genericProd splitL

genericPower :: Splitter a -> Spe a b -> Int -> Spe a [b]
genericPower h f k = genericProd h $ replicate k f

-- | The power F^k for species F
power = genericPower splitB

-- | The ordinal power F^k for L-species F
powerL = genericPower splitL

-- | The composition F(G) of two species F and G
compose :: Spe [a] b -> Spe a c -> Spe a (b, [c])
compose f g xs = [ (y, ys) | bs <- par xs, y <- f bs, ys <- mapM g bs ]

-- | This is just a synonym for `compose`. It is usually used infix.
o = compose

-- | The derivative d^k/dX^k F of a species F
kDiff :: Int -> Spe (Maybe a) b -> Spe a b
kDiff k f xs = f $ replicate k Nothing ++ map Just xs

-- | The first derivative
diff = kDiff 1

-- | The species of sets
set :: Spe a [a]
set = return

-- | The species characteristic of the empty set; the identity with
-- respect to species multiplication.
one :: Spe a [a]
one [] = [[]]
one _  = []

-- | The singleton species
x :: Spe a [a]
x xs@[_] = [xs]
x _      = []

-- | f `ofSize` n is like f on n element sets, but empty otherwise.
ofSize :: Spe a c -> Int -> Spe a c
(f `ofSize` n) xs | xs `isOfLength` n = f xs
                  | otherwise         = []

-- Like length xs == n, but lazy
isOfLength :: [a] -> Int -> Bool
[]     `isOfLength` n = n == 0
(x:xs) `isOfLength` n = n > 0 && xs `isOfLength` (n-1)

-- | No structure on the empty set, but otherwise the same.
nonEmpty :: Spe a c -> Spe a c
nonEmpty _ [] = []
nonEmpty f xs = f xs

-- | The species of ballots with k blocks
kBal :: Int -> Spe a [[a]]
kBal 0 = \xs -> [ [] | null xs ]
kBal k = nonEmpty set `power` k

-- | The species of ballots
bal :: Spe a [[a]]
bal [] = [[]]
bal xs = [ b:bs | (b, ys) <- init (splitB xs), bs <- bal ys ]

-- | The species of set partitions
par :: Spe a [[a]]
par [] = [[]]
par (x:xs) = [ (x:b) : bs | (b, ys) <- splitB xs, bs <- par ys ]

-- | The species of lists (linear orders) with k elements
kList :: Int -> Spe a [a]
kList 0 = one
kList k = map concat . (x `power` k)

-- | The species of lists
list :: Spe a [a]
list xs = kList (length xs) xs

-- | The species of cycles
cyc :: Spe a [a]
cyc [] = []
cyc (x:xs) = map (x:) $ list xs

-- | The species of permutations, where a permutation is a set of
-- cycles.
perm :: Spe a [[a]]
perm = map fst . (set `o` cyc)

-- | The species of k element subsets
kSubsets :: Int -> Spe a ([a], [a])
kSubsets k = (set `ofSize` k) `mul` set

-- | The species of subsets
subsets :: Spe a ([a], [a])
subsets = set `mul` set
