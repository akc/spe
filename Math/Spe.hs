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
    , (.+.), assemble, (.*.), (<*.), prod, ordinalProd, (.^), (<^), o, dX
    , ofSize, nonEmpty
    -- * Specific species
    , set, one, x, kBal, bal, par, kList, list, cyc, perm, kSubset, subset
    ) where

import Data.List
import Control.Monad
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

type Splitter a = [a] -> [([a], [a])]

-- Constructions
-- -------------

decompose :: Splitter a -> Int -> [a] -> [[[a]]]
decompose _ 0 [] = [[]]
decompose _ 0 _  = []
decompose h k xs = [ b:bs | (b,ys) <- h xs, bs <- decompose h (k-1) ys ]

-- A splitter for L-species.
splitL :: Splitter a
splitL [] = [([], [])]
splitL xs@(x:xt) = ([], xs) : [ (x:as, bs) | (as, bs) <- splitL xt ]

-- A splitter for B-species.
splitB :: Splitter a
splitB [] = [([], [])]
splitB (x:xs) = splitB xs >>= \(ys, zs) -> [(x:ys, zs), (ys, x:zs)]

-- | Species addition.
(.+.) :: Spe a b -> Spe a c -> Spe a (Either b c)
(.+.) f g xs = (Left <$> f xs) ++ (Right <$> g xs)

-- | The sum of a list of species of the same type.
assemble :: [Spe a c] -> Spe a c
assemble fs xs = fs >>= \f -> f xs

genericMul :: Splitter a -> Spe a b -> Spe a c -> Spe a (b,c)
genericMul h f g xs = h xs >>= \(ys,zs) -> (,) <$> f ys <*> g zs

-- | Species multiplication.
(.*.) = genericMul splitB

-- | Ordinal L-species multiplication. Give that the underlying set is
-- sorted , elements in the left factor will be smaller than those in
-- the right factor.
(<*.) = genericMul splitL

genericProd :: Splitter a -> [Spe a b] -> Spe a [b]
genericProd h fs xs =
    let n = length fs
    in [ zipWith ($) fs bs | bs <- decompose h n xs ] >>= sequence

-- | The product of a list of species.
prod = genericProd splitB

-- | The ordinal product of a list of L-species.
ordinalProd = genericProd splitL

genericPower :: Splitter a -> Spe a b -> Int -> Spe a [b]
genericPower h f k = genericProd h $ replicate k f

-- | The power F^k for species F.
(.^) = genericPower splitB

-- | The ordinal power F^k for L-species F.
(<^) = genericPower splitL

-- | The (partitional) composition F(G) of two species F and G. It is
-- usually used infix.
o :: Spe [a] b -> Spe a c -> Spe a (b, [c])
o f g xs = [ (y, ys) | bs <- par xs, y <- f bs, ys <- mapM g bs ]

-- | The derivative d/dX F of a species F.
dX :: Spe (Maybe a) b -> Spe a b
dX f xs = f $ Nothing : (Just <$> xs)

-- Like length xs == n, but lazy.
isOfLength :: [a] -> Int -> Bool
[]     `isOfLength` n = n == 0
(x:xs) `isOfLength` n = n > 0 && xs `isOfLength` (n-1)

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
kBal 0 = \xs -> [ [] | null xs ]
kBal k = nonEmpty set .^ k

-- | The species of ballots.
bal :: Spe a [[a]]
bal [] = [[]]
bal xs = [ b:bs | (b, ys) <- init (splitB xs), bs <- bal ys ]

-- | The species of set partitions.
par :: Spe a [[a]]
par [] = [[]]
par (x:xs) = [ (x:b) : bs | (b, ys) <- splitB xs, bs <- par ys ]

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
subset = map fst . splitB
