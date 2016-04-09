module Gen where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

-- new QuickCheck interface is a bit different, see https://hackage.haskell.org/package/QuickCheck-2.8.1/docs/Test-QuickCheck.html https://hackage.haskell.org/package/QuickCheck-2.8.1/docs/Test-QuickCheck-Random.html
-- see http://blog.jb55.com/post/6180072300/using-haskells-quickcheck-to-generate-random-test-data
-- see http://www.haskell.org/haskellwiki/QuickCheck_as_a_test_set_generator

data Serial = Serial Int String

instance Show Serial where
  show (Serial number string) = (show number) ++ ":" ++ string

instance Arbitrary Serial where
  arbitrary = do
    string <- vectorOf 7 $ elements ['A'..'Z']
    number <- choose (1000, 9999)
    return $ Serial number string 

serialGen :: Int -> [Serial]
serialGen seed = unGen arbitrary (mkQCGen seed) 9999999

keyValue :: Serial -> (Int, String)
keyValue (Serial number string) = (number, string)

kvGen :: Int -> Int -> [(Int, String)]
kvGen seed n = take n $ map keyValue $ serialGen seed
-- was ist übrigens die point-free Version von kvGen?