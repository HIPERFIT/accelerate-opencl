module Fold_Test (tests) where

import Data.Array.Accelerate (Acc, Exp, Elt, Z(..), (:.)(..))
import qualified Data.Array.Accelerate as Acc
import qualified Data.Array.Accelerate.Smart as Acc
import qualified Data.Array.Accelerate.OpenCL as Acc

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

-- Convert seconds to milliseconds
seconds n = n * 1000000
milliseconds n = n * 1000

-- Make problems larger, and limit the time used on solving each problem
scale :: Testable a => a -> Property
scale = within (milliseconds 400) . mapSize (*10) . property

tests = testGroup "fold"
        [        
          testProperty "fold sum, empty list" test_fold_nil_tuple
        , testProperty "fold sum, empty list" test_fold_nil
        , testProperty "fold sum, Int" (scale test_fold_sum)
        , testProperty "fold sum, (Int, Int)" (scale test_fold_sumTuple)
        , testProperty "fold1 sum, Int" (scale test_fold1_sum)
        , testProperty "fold1 sum, (Int, Int)" (scale test_fold1_sumTuple)
        ]

-- Simultaneous addition of tuples
add_simple (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

add :: Exp (Int, Int) -> Exp (Int, Int) -> Exp (Int, Int)
add x y = let (x1, y1) = Acc.unlift x :: (Exp Int, Exp Int)
              (x2, y2) = Acc.unlift y :: (Exp Int, Exp Int)
          in Acc.lift ( x1 + x2, y1 + y2)

-- fold1 tests --
test_fold1_sum :: [Int] -> Bool
test_fold1_sum = test_fold1 (+) (+)

test_fold1_sumTuple :: [(Int, Int)] -> Bool
test_fold1_sumTuple = test_fold1 add_simple add

-- fold tests --

-- We do not want to test whether fold on empty list here
test_fold_sum :: Int -> [Int] -> Property
test_fold_sum x xs = (not $ null xs) ==> test_fold (+) (+) x xs

test_fold_sumTuple :: (Int, Int) -> [(Int, Int)] -> Property
test_fold_sumTuple x xs = (not $ null xs) ==> test_fold add_simple add x xs

-- Tests the empty list case
test_fold_nil :: Property
test_fold_nil = 
  mapSize (const 1) $ test_fold (+) (+) (0 :: Int) []

test_fold_nil_tuple :: Property
test_fold_nil_tuple = 
  mapSize (const 1) $ test_fold add_simple add ((0,0) :: (Int, Int)) []


-- Generic test functions
-- | Generic fold1 test
--
-- Supply it with a regular function and an Accelerate function that
-- performs the same operations, to create a QuickCheckable property
test_fold1 :: (Eq a, Elt a)
           => (a -> a -> a) -> (Exp a -> Exp a -> Exp a) -> [a] -> Bool
test_fold1 _ _ [] = True -- foldl1 errors on empty list
test_fold1 f f_acc xs = Prelude.foldl1 f xs == (head . Acc.toList $ Acc.run (doFold vector))
  where
    doFold = Acc.fold1 f_acc . Acc.use
    vector = Acc.fromList (Z :. length xs) xs

-- | Generic fold test
--
-- Supply it with a regular function and an Accelerate function that
-- performs the same operations, to create a QuickCheckable property
test_fold :: (Eq a, Elt a)
          => (a -> a -> a) -> (Exp a -> Exp a -> Exp a) -> a -> [a] -> Bool
test_fold f f_acc x xs = Prelude.foldl f x xs == (head . Acc.toList $ Acc.run (doFold vector))
  where
    doFold = Acc.fold f_acc (Acc.constant x) . Acc.use
    vector = Acc.fromList (Z :. length xs) xs
