module Data.Root ( (-/)
                 , toFloating
                 , gr ) where

import Data.Ratio          ( (%)
                           , denominator
                           , numerator    )
import Data.List           ( group
                           , partition    )
import Data.Numbers.Primes ( primeFactors )

data Root' = Rational :-/ [Integer]
             deriving ( Eq )

data Root  = Zero
           | !Root' :+ !Root
             deriving ( Eq )

instance Show Root' where
    show (a :-/ b) | null b             = if denominator a == 1
                                             then show (numerator a)
                                             else show a
                   | denominator a == 1 = show (numerator a)
                                          ++ " -/ "
                                          ++ show (product b)
                   | otherwise          = "("
                                          ++ show (numerator a)
                                          ++ " -/ "
                                          ++ show (product b)
                                          ++ ") % "
                                          ++ show (denominator a)

instance Show Root where
    show Zero     = "Zero"
    show (a :+ b) = f a b
                    where f x Zero     = show x
                          f x (y :+ z) = show x ++ " + " ++ f y z

(-/)   :: Rational -> Integer -> Root
a -/ b =  ((a * (c % 1)) :-/ ps) :+ Zero
          where allps   = group $ primeFactors b
                ptimes  = zip (map head   allps) -- [(prime, times)]
                              (map length allps) -- 18 -> [(2, 1), (3, 2)]
                                                 -- means 2 ^ 1 + 3 ^ 2
                (c, ps) = go 1 [] ptimes
                go                    :: Integer
                                      -> [Integer]
                                      -> [(Integer, Int)]
                                      -> (Integer, [Integer])
                go r ps []                         = (r, ps)
                go r ps ((p, t) : xs) |  t == 1    = go r
                                                        (p : ps)
                                                        xs
                                      |  otherwise = go (r * (p ^ d))
                                                        ((if m == 0
                                                             then id
                                                             else (p :)) ps)
                                                        xs
                                                     where (d, m) = t `divMod` 2

instance Num Root where
    a    + Zero      = a
    Zero + a         = a
    a    + (b :+ rs) = f a b + rs
                       where f                                         :: Root -> Root' -> Root
                             f Zero z                                               =  z :+ Zero
                             f (rt1@(c1 :-/ r1) :+ rs) rt2@(c2 :-/ r2) |  r1 == r2  =  ((c1 + c2) :-/ r1) :+ rs
                                                                       |  otherwise =  rt1 :+ f rs rt2

    _    * Zero      = Zero
    Zero * _         = Zero
    a    * (r :+ rs) = f a r + a * rs
                       where f             :: Root -> Root' -> Root
                             f Zero z      =  Zero
                             f (a :+ b) c  =  g a c + f b c
                             g :: Root' -> Root' -> Root
                             g (c1 :-/ r1) (c2 :-/ r2) = (c1 * c2) -/ product (r1 ++ r2)

    negate Zero             = Zero
    negate ((c :-/ r) :+ rs) = ((- c) :-/ r) :+ negate rs

    fromInteger a = (a % 1) -/ 1

    signum r | fr > 0    =   1
             | fr < 0    = - 1
             | otherwise =   0
               where fr = toFloating r

    abs r = r * signum r

toFloating :: Floating a => Root -> a
toFloating              Zero =  0
toFloating ((c :-/ r) :+ rs) =  fromRational c * sqrt (fromIntegral $ product r) + toFloating rs

gr :: Root
gr =  ((1 % 2) -/ 1) + ((1 % 2) -/ 5)
