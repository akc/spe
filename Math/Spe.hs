-- |
-- Copyright   : Anders Claesson 2014
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
-- Species lite. See <http://github.com/akc/spe>
-- for an introduction and examples.

module Math.Spe
    (
    -- * The species type synonym
      Spe
    -- * Constructions
    , (.+.), assemble, (.*.), (<*.), prod, ordProd, (.^), (<^), (><), o, ordComp
    , dx, pointed, ofSize, nonEmpty
    -- * Contact of order n
    , contact
    -- * Specific species
    , set, one, x, kBal, bal, par, kList, list, cyc, perm, kSubset, subset
    , parL
    ) where

import Data.List
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

type BiPar a = Spe a ([a], [a])

-- Constructions
-- -------------

-- | Species addition.
(.+.) :: Spe a b -> Spe a c -> Spe a (Either b c)
(.+.) f g us = (Left <$> f us) ++ (Right <$> g us)

-- | The sum of a list of species of the same type.
assemble :: [Spe a c] -> Spe a c
assemble fs us = fs >>= \f -> f us

-- A bipartition for L-species.
biparL :: BiPar a
biparL [] = [([], [])]
biparL us@(u:ut) = ([], us) : [ (u:vs, zs) | (vs, zs) <- biparL ut ]

-- A bipartition for B-species.
biparB :: BiPar a
biparB [] = [([], [])]
biparB (u:us) = biparB us >>= \(vs, zs) -> [(u:vs, zs), (vs, u:zs)]

-- Generic species multiplication.
mul :: BiPar a -> Spe a b -> Spe a c -> Spe a (b,c)
mul h f g us = h us >>= \(vs,zs) -> (,) <$> f vs <*> g zs

-- | Species multiplication.
(.*.) :: Spe a b -> Spe a c -> Spe a (b, c)
(.*.) = mul biparB

-- | Ordinal L-species multiplication. Given that the underlying set is
-- sorted, elements in the left factor will be smaller than those in the
-- right factor.
(<*.) :: Spe a b -> Spe a c -> Spe a (b, c)
(<*.) = mul biparL

-- Generic species product. The definition below is equivalent to
-- > prod' h = foldr (\f g -> map (uncurry (:)) . mul h f g) one
-- but a bit more efficient.
prod' :: BiPar a -> [Spe a b] -> Spe a [b]
prod' h fs us = zipWith ($) fs <$> kEnd h (length fs) us >>= sequence

-- Preimages of endo functions [1..k] -> us. (Used in prod'.)
kEnd :: BiPar a -> Int -> Spe a [[a]]
kEnd _ 0 [] = [[]]
kEnd _ 0 _  = []
kEnd h k us = h us >>= \(b,vs) -> (b:) <$> kEnd h (k-1) vs

-- Generic species power function, using peasant multiplication.
power :: BiPar a -> Spe a b -> Int -> Spe a [b]
power _ _ k | k < 0 = error "Negative exponent"
power _ _ 0         = one
power _ f 1         = map return . f
power h f k         = map concat . prod' h [power h f j, g, g]
  where
    (i,j) = divMod k 2; g = power h f i

-- | The product of a list of species.
prod :: [Spe a b] -> Spe a [b]
prod = prod' biparB

-- | The ordinal product of a list of L-species.
ordProd :: [Spe a b] -> Spe a [b]
ordProd = prod' biparL

-- | The power F^k for species F.
(.^) :: Spe a b -> Int -> Spe a [b]
(.^) = power biparB

-- | The ordinal power F^k for L-species F.
(<^) :: Spe a b -> Int -> Spe a [b]
(<^) = power biparL

-- | The Cartesian product of two species.
(><) :: Spe a b -> Spe a c -> Spe a (b,c)
(><) f g us = (,) <$> f us <*> g us

-- | The (partitional) composition F(G) of two species F and G. It is
-- usually used infix.
o :: Spe [a] b -> Spe a c -> Spe a (b, [c])
o f g us = par us >>= f >< mapM g

-- | The (partitional) L-species composition of two species.
ordComp :: Spe [a] b -> Spe a c -> Spe a (b, [c])
ordComp f g us = parL us >>= f >< mapM g

-- | The derivative d/dX F of a species F.
dx :: Spe (Maybe a) b -> Spe a b
dx f us = f $ Nothing : (Just <$> us)

-- | The pointing operator.
pointed :: Spe a b -> Spe a (b, a)
pointed f = f >< id

-- Like length us == n, but lazy.
isOfLength :: [a] -> Int -> Bool
[]     `isOfLength` n = n == 0
(_:us) `isOfLength` n = n > 0 && us `isOfLength` (n-1)

-- | f `ofSize` n is like f on n element sets, but empty otherwise.
ofSize :: Spe a c -> Int -> Spe a c
(f `ofSize` n) us | us `isOfLength` n = f us
                  | otherwise         = []

-- | No structure on the empty set, but otherwise the same.
nonEmpty :: Spe a c -> Spe a c
nonEmpty _ [] = []
nonEmpty f us = f us

-- Contact of order n
-- ------------------

-- | Check whether two species have contact of order n.
contact :: Ord b => Int -> Spe Int b -> Spe Int b -> Bool
contact n f g = and [ sort (f [1..k]) == sort (g [1..k]) | k<-[1..n] ]

-- Specific species
-- ----------------

-- | The species of sets.
set :: Spe a [a]
set = return

-- | The species characteristic of the empty set; the identity with
-- respect to species multiplication.
one :: Spe a [b]
one us = [ [] | null us ]

-- | The singleton species.
x :: Spe a a
x = id `ofSize` 1

-- | The species of ballots with k blocks.
kBal :: Int -> Spe a [[a]]
kBal k = nonEmpty set .^ k

-- | The species of ballots.
bal :: Spe a [[a]]
bal [] = [[]]
bal us = [ b:bs | (b, vs) <- init (biparB us), bs <- bal vs ]

-- | The species of set partitions.
par :: Spe a [[a]]
par [] = [[]]
par (u:us) = [ (u:b) : bs | (b, vs) <- biparB us, bs <- par vs ]

-- | The L-species of set partitions.
parL :: Spe a [[a]]
parL [] = [[]]
parL (u:us) = [ (u:b) : bs | (b, vs) <- biparL us, bs <- parL vs ]

-- | The species of lists (linear orders) with k elements.
kList :: Int -> Spe a [a]
kList k = x .^ k

-- | The species of lists (linear orders).
list :: Spe a [a]
list us = kList (length us) us

-- | The species of cycles.
cyc :: Spe a [a]
cyc [] = []
cyc (u:us) = (u:) <$> list us

-- | The species of permutations (sets of cycles).
perm :: Spe a [[a]]
perm = map fst . (set `o` cyc)

-- | The species of k element subsets.
kSubset :: Int -> Spe a [a]
kSubset k = map fst . (set `ofSize` k .*. set)

-- | The species of subsets. The definition given here is equivalent to
-- @subset = map fst . (set .*. set)@, but a bit more efficient.
subset :: Spe a [a]
subset = map fst . biparB
