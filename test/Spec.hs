import Test.Hspec
import Test.Hrubric

import System.Environment

-- normal rubric, should evaluate to
rubric0 :: Rubric
rubric0 = do
  criterion "Nested" 0.7 . distribute $ do
    dcriterion "fn0" . passOrFail $ do
      it "bogus0" $ do
        True `shouldBe` True
      it "bogus1" $ do
        False `shouldBe` True
    dcriterion "fn1" . passOrFail $ do
      it "bogus0" $ do
        True `shouldBe` True
  criterion "fn2" 0.2 $ do
    passes "bogus0" 0.3 $ do
      True `shouldBe` True
    passes "bogus1" 0.7 $ do
      False `shouldBe` True
  criterion "fn3" 0.1 . distribute $ do
    dpasses "bogus0" $ do
      True `shouldBe` True
    dpasses "bogus1" $ do
      True `shouldBe` True

-- bad rubric, should crash on assembly
rubric1 :: Rubric
rubric1 = do
  criterion "Upper" 1.0 $ do
    criterion "Big" 0.8 $ do
      passes "x" 1 $ do
        True `shouldBe` True
    criterion "Too big" 0.4 $ do
      passes "x" 1 $ do
        True `shouldBe` True

tests :: Spec
tests = do
  it "runs a passing rubric with correct grade" $ do
    hrubric rubric0 `shouldReturn` Just 0.51
  it "does not grade on partial rubric evaluation" $ do
    withArgs ["-m fn0"] (hrubric rubric0) `shouldReturn` Nothing
  it "fails early on a bad rubric" $ do
    hrubric rubric1 `shouldThrow` anyException

main :: IO ()
main = hspec tests
