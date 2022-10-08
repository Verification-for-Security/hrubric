# HRubric

A small Hspec extension that allows one to place weights on
tests. This way, one can easily create an auto-grader for
a programming assignment!

Basic usage:

```Haskell
rubric :: Rubric
rubric = do
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

main = hrubric rubric
```
