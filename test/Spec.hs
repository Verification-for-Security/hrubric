import Test.Hspec
import Test.Hrubric

-- normal rubric, should evaluate to
rubric0 :: Rubric
rubric0 = do
  criterion "Nested" 0.8 . distribute $ do
    distributed "fn0" . passOrFail $ do
      it "bogus0" $ do
        True `shouldBe` True
      it "bogus1" $ do
        False `shouldBe` True
    distributed "fn1" . passOrFail $ do
      it "bogus0" $ do
        True `shouldBe` True
  criterion "fn2" 0.2 $ do
    passes "bogus0" 0.3 $ do
      True `shouldBe` True
    passes "bogues1" 0.7 $ do
      False `shouldBe` True

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
    hrubric rubric0 `shouldReturn` Right 0.46
  it "fails early on a bad rubric" $ do
    hrubric rubric1 `shouldReturn` Left "Upper"

main :: IO ()
main = hspec tests
