-- |
-- Copyright   : Anders Claesson 2014
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
-- Species lite. See <http://github.com/akc/spe> for an introduction and examples.

module Math.Spe
    (
    -- * The species type synonym
      Spe
    -- * Constructions
    , (.+.), assemble, (.*.), (<*.), prod, ordProd, (.^), (<^), (><), o
    , dx, pointed, ofSize, nonEmpty
    -- * Specific species
    , set, one, x, kBal, bal, par, kList, list, cyc, perm, kSubset, subset
    ) where

import Control.Applicative

infixl 6 .+.
infixl 7 .*.
infixl 7 <*.
infixr 8 .^
infixr 8 <^

-- The species type synonym
-- ------------------------

-- | A
-- <https://en.wikipedia.org/wiki/Combinatorial_species combinatorial species>
-- is an endofunctor on the category of finite sets and bijections. We
-- approximate this by a function as defined.
type Spe a c = [a] -> [c]

type Bipartition a = [a] -> [([a], [a])]

-- Constructions
-- -------------

-- | Species addition.
(.+.) :: Spe a b -> Spe a c -> Spe a (Either b c)
(.+.) f g xs = (Left <$> f xs) ++ (Right <$> g xs)

-- | The sum of a list of species of the same type.
assemble :: [Spe a c] -> Spe a c
assemble fs xs = fs >>= \f -> f xs

-- Preimages of endo functions [1..k] -> [1..n]
kEndBy :: Bipartition a -> Int -> [a] -> [[[a]]]
kEndBy _ 0 [] = [[]]
kEndBy _ 0 _  = []
kEndBy h k xs = h xs >>= \(b,ys) -> (b:) <$> kEndBy h (k-1) ys

-- A bipartition for L-species.
bipartL :: Bipartition a
bipartL [] = [([], [])]
bipartL xs@(x:xt) = ([], xs) : [ (x:ys, zs) | (ys,zs) <- bipartL xt ]

-- A bipartition for B-species.
bipartB :: Bipartition a
bipartB [] = [([], [])]
bipartB (x:xs) = bipartB xs >>= \(ys, zs) -> [(x:ys, zs), (ys, x:zs)]

-- Generic species multiplication.
mulBy :: Bipartition a -> Spe a b -> Spe a c -> Spe a (b,c)
mulBy h f g xs = h xs >>= \(ys,zs) -> (,) <$> f ys <*> g zs

-- | Species multiplication.
(.*.) :: Spe a b -> Spe a c -> Spe a (b, c)
(.*.) = mulBy bipartB

-- | Ordinal L-species multiplication. Give that the underlying set is
-- sorted , elements in the left factor will be smaller than those in
-- the right factor.
(<*.) :: Spe a b -> Spe a c -> Spe a (b, c)
(<*.) = mulBy bipartL

prodBy :: Bipartition a -> [Spe a b] -> Spe a [b]
prodBy h fs xs = zipWith ($) fs <$> kEndBy h (length fs) xs >>= sequence

-- | The product of a list of species.
prod :: [Spe a b] -> Spe a [b]
prod = prodBy bipartB

-- | The ordinal product of a list of L-species.
ordProd :: [Spe a b] -> Spe a [b]
ordProd = prodBy bipartL

powerBy :: Bipartition a -> Spe a b -> Int -> Spe a [b]
powerBy h f k = prodBy h $ replicate k f

-- | The power F^k for species F.
(.^) :: Spe a b -> Int -> Spe a [b]
(.^) = powerBy bipartB

-- | The ordinal power F^k for L-species F.
(<^) :: Spe a b -> Int -> Spe a [b]
(<^) = powerBy bipartL

-- | The Cartesian product of two species,
(><) :: Spe a b -> Spe a c -> Spe a (b,c)
(><) f g xs = (,) <$> f xs <*> g xs

-- | The (partitional) composition F(G) of two species F and G. It is
-- usually used infix.
o :: Spe [a] b -> Spe a c -> Spe a (b, [c])
o f g xs = par xs >>= f >< mapM g

-- | The derivative d/dX F of a species F.
dx :: Spe (Maybe a) b -> Spe a b
dx f xs = f $ Nothing : (Just <$> xs)

-- | The pointing operator.
pointed :: Spe a b -> Spe a (b, a)
pointed f = f >< id

-- Like length xs == n, but lazy.
isOfLength :: [a] -> Int -> Bool
[]     `isOfLength` n = n == 0
(_:xs) `isOfLength` n = n > 0 && xs `isOfLength` (n-1)

-- | f `ofSize` n is like f on n element sets, but empty otherwise.
ofSize :: Spe a c -> Int -> Spe a c
(f `ofSize` n) xs | xs `isOfLength` n = f xs
                  | otherwise         = []

-- | No structure on the empty set, but otherwise the same.
nonEmpty :: Spe a c -> Spe a c
nonEmpty _ [] = []
nonEmpty f xs = f xs


-- Specific species
-- ----------------

-- | The species of sets.
set :: Spe a [a]
set = return

-- | The species characteristic of the empty set; the identity with
-- respect to species multiplication.
one :: Spe a ()
one xs = [ () | null xs ]

-- | The singleton species.
x :: Spe a a
x = id `ofSize` 1

-- | The species of ballots with k blocks
kBal :: Int -> Spe a [[a]]
kBal k = nonEmpty set .^ k

-- | The species of ballots.
bal :: Spe a [[a]]
bal [] = [[]]
bal xs = [ b:bs | (b, ys) <- init (bipartB xs), bs <- bal ys ]

-- | The species of set partitions.
par :: Spe a [[a]]
par [] = [[]]
par (x:xs) = [ (x:b) : bs | (b, ys) <- bipartB xs, bs <- par ys ]

-- | The species of lists (linear orders) with k elements.
kList :: Int -> Spe a [a]
kList k = x .^ k

-- | The species of lists (linear orders)
list :: Spe a [a]
list xs = kList (length xs) xs

-- | The species of cycles.
cyc :: Spe a [a]
cyc [] = []
cyc (x:xs) = (x:) <$> list xs

-- | The species of permutations (sets of cycles).
perm :: Spe a [[a]]
perm = map fst . (set `o` cyc)

-- | The species of k element subsets.
kSubset :: Int -> Spe a [a]
kSubset k = map fst . (set `ofSize` k .*. set)

-- | The species of subsets. The definition given here is equivalent to
-- @subset = map fst . (set .*. set)@, but a bit faster.
subset :: Spe a [a]
subset = map fst . bipartB
