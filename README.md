# hrubric

A small hspec extension that allows one to place weights on
tests. This way, one can create an auto-grader for
a Haskell programming assignment!

## Example

```Haskell
rubric :: Rubric
rubric = do
  criterion "Nested" 0.8 . distribute $ do
    dcriterion "fn0" . passOrFail $ do
      it "bogus0" $ do
        True `shouldBe` True
      it "bogus1" $ do
        False `shouldBe` True
    dcriterion "fn1" $ do
      dpasses "bogus0" $ do
        True `shouldBe` True
      dpasses "bogus0" $ do
        True `shouldBe` True
  criterion "fn2" 0.2 $ do
    passes "bogus0" 0.3 $ do
      True `shouldBe` True
    passes "bogues1" 0.7 $ do
      False `shouldBe` True

main = hrubric rubric >>= print
```

The grade will be a Float in the range 0.0 to 1.0.
Of course, you can scale this to the grading scheme
used in your educational institute!

If the accumulated weight of some part of your tree
was not equal to 1.0, then this will instead give an
error with a path to the branch that had poor weights.
