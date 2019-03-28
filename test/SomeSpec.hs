module SomeSpec (spec) where

import Test.Hspec
import Some

spec :: Spec
spec = do
  describe "fizzbuzz" $ do
    it "数値を文字列にして返す" $ do
      fizzbuzz 1 `shouldBe` "1"
      fizzbuzz 2 `shouldBe` "2"

    it "3の倍数の場合はFizzを返す" $ do
      fizzbuzz 3 `shouldBe` "Fizz"
      fizzbuzz 6 `shouldBe` "Fizz"

    it "5の倍数の場合はBuzzを返す" $ do
      fizzbuzz 5 `shouldBe` "Buzz"
      fizzbuzz 10 `shouldBe` "Buzz"

    it "3と5両方の倍数の場合はFizzBuzzを返す" $ do
      fizzbuzz 15 `shouldBe` "FizzBuzz"
      fizzbuzz 30 `shouldBe` "FizzBuzz"

  describe "factorial" $ do
    let test arg expected = it (show arg) $ factorial arg `shouldBe` expected
    test 0 1
    test 1 1
    test 5 120

  describe "sum'" $ do
    let test arg expected = it (show arg) $ sum' arg `shouldBe` expected
    test [] 0
    test [1] 1
    test [1..10] 55

  describe "length'" $ do
    let test arg expected = it (show arg) $ length' arg `shouldBe` expected
    test ([]::[Int]) 0
    test [1] 1
    test [1..10] 10

  describe "any'" $ do
    let test arg expected = it (show (take 20 arg)) $ any' (> 5) arg `shouldBe` expected
    test [] False
    test [1] False
    test [1..5] False
    test [1..10] True
    test [1..] True

  describe "all'" $ do
    let test arg expected = it (show (take 20 arg)) $ all' (<= 5) arg `shouldBe` expected
    test [] True
    test [1] True
    test [1..5] True
    test [1..10] False
    test [1..] False

  describe "maximum'" $ do
    let test arg expected = it (show arg) $ maximum' arg `shouldBe` expected
    test [0] 0
    test [1] 1
    test [5..10] 10
    test [2,30,4,7,19] 30

  describe "take'" $ do
    let test arg@(n, l) expected = it (show arg) $ take' n l `shouldBe` expected
    test (0, [1..10]) []
    test (-1, [1..10]) []
    test (1, [1..10]) [1]
    test (5, [1..10]) [1,2,3,4,5]

  describe "reverse'" $ do
    let test arg expected = it (show arg) $ reverse' arg `shouldBe` expected
    test [] ([] :: [Int])
    test [1] [1]
    test [1..5] [5,4,3,2,1]

  describe "last'" $ do
    let test arg expected = it (show arg) $ last' arg `shouldBe` expected
    test [1] 1
    test [1,2] 2
    test [1..5] 5

  describe "elem'" $ do
    let test arg@(e, l) expected = it (show arg) $ elem' e l `shouldBe` expected
    test (0, []) False
    test (0, [1..10]) False
    test (0, [0,2,3,4,5]) True
    test (0, [1,2,0,4,5]) True
    test (0, [1,2,3,4,0]) True

  describe "map'" $ do
    let test arg expected = it (show arg) $ map' (* 2) arg `shouldBe` expected
    test [] []
    test [1] [2]
    test [1..5] [2,4,6,8,10]

  describe "filter'" $ do
    let test arg expected = it (show arg) $ filter' odd arg `shouldBe` expected
    test [] ([] :: [Int])
    test [2] []
    test [1] [1]
    test [1..5] [1,3,5]

  describe "reduce" $ do
    let test arg@(i, l) expected = it (show arg) $ reduce (+) i l `shouldBe` expected
    test (0, []) 0
    test (1, [1]) 2
    test (0, [1..10]) 55

