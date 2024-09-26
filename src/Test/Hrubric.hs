module Test.Hrubric
  ( Criteria
  , Criterion (..)
  , grade

  , RubricM
  , runRubric
  , evalRubric

  , Rubric
  , hrubric
  , criterion
  , passes
  , dcriterion
  , dpasses
  , bcriterion
  , bpasses
  , distribute
  , passOrFail
  ) where

import Test.Hspec
import Test.Hspec.Core.Spec
import Test.Hspec.Runner hiding (Path)
import System.Environment

import Data.IORef
import Data.List (isPrefixOf, intercalate)
import Data.Maybe (isJust)

import Control.Monad.Writer
import Control.Arrow
import Control.Applicative (empty, (<|>))

-- | The list of criteria in a rubric.
-- The final weigth of the criteria
-- should add up to 1.0 (or just be
-- distribute for all instances)
type Criteria = [Criterion]

-- | A criterion in a rubric. Contains
-- nested rubrics in a tree to place
-- weights on sub-criteria
data Criterion = Criterion
  { name :: String
  -- Name of this criterion. Useful to trace incorrectness in rubrics.
  , weight :: Float
  -- ^ The weight of this criterion.
  , base :: Float
  -- ^ Base line on the grade. Example would be for multiple choice, where we
  -- want to correct the grade to account for guesses. If we had 4 choices per
  -- question, we would set the base to 0.25. I.e. if you have 25% of this
  -- rubric correct, you get 0 pts.
  , nodes  :: Criteria
  -- ^ Empty rubric means pass or fail
  }
  deriving (Show, Eq)

-- | A path to a test
type Path = [String]

grade :: Criterion -> SpecResult -> Float
grade criteria result = recurse [] criteria
  where
    items = specResultItems result

    recurse path Criterion { nodes = [], weight, name } = do
      let path' = path <> [name]
      let matches = filter (subpath path') items
      let isSucces item = case resultItemStatus item of
            ResultItemSuccess -> True
            _ -> False
      -- No points if there is a failure. A test case wasn't run if there
      -- were no matches, so we don't award points in that case.
      if null matches || not (all isSucces matches)
        then 0
        else weight
    recurse path Criterion { nodes, weight, name, base } = weighted
      where
        weighted = withBase * weight
        withBase = max (summed - base) 0 / (1 - base)
        summed = sum grades
        grades = recurse path' <$> nodes
        path' = path <> [name]

    -- Check if a Path path matches the item path
    subpath :: Path -> ResultItem -> Bool
    subpath p q = do
      let (q', n) = resultItemPath q
      p `isPrefixOf` (q' <> [n])

-- | Sanity check of criteria.
--
-- Do a sanity check on the criteria, returning a path to the set of criteria
-- that do not match if this exists.
sanity :: Criteria -> Either [String] ()
sanity cs
  | sane cs   = foldlM' (\_ -> sanity . nodes) cs
  | otherwise = Left []
  where
    -- We have a specific foldM here that
    -- attaches the path that failed (if we
    -- had a path with bad weights)
    foldlM' f xs = foldr c return xs ()
      where
        c x k z = left (name x :) (f z x) >>= k

-- | Check if this layer is sane.
--
-- Check sanity (i.e. total weight of 1) of one layer in the tree (so no
-- recursive check) Empty tree is always sane, as it is treated as a pass or
-- fail.
sane :: Criteria -> Bool
sane [] = True
sane c  = all inRange bases && all inRange weights && cmp total 1
  where
    inRange f = 0 <= f && f <= 1

    bases = fmap base c

    total = sum weights
    weights = fmap weight c

    -- Adjust for floating point errors
    cmp x y = abs (x-y) < 0.0001

-- | Rubric monad.
type RubricM r = WriterT Criteria (SpecM r)

type Rubric = RubricM () ()

-- withIORef :: SpecM r a -> 

-- | Run the full rubric
--
-- This is analogous to the hspec function.
hrubric :: Rubric -> IO (Maybe Float)
hrubric rubric = do
  -- Set up an IO ref to get the criteria out of the hspec. We do this because
  -- hspec doesn't allow for non-unit type outputs when running the monad.
  ref <- newIORef []
  let spec = do
        (a, criteria) <- runRubric rubric
        runIO $ writeIORef ref criteria
        return a

  args <- getArgs
  cfg <- readConfig defaultConfig args
  (cfg', forest) <- evalSpec cfg spec
  result <- liftIO . withArgs [] $ runSpecForest forest cfg'

  criteria <- readIORef ref
  let grades = flip grade result <$> criteria
  return $ if isJust (configFilterPredicate cfg <|> configSkipPredicate cfg)
    then empty
    else return $ sum grades

-- | Run the rubric monad. This incorporates a sanity check.
runRubric :: RubricM s a -> SpecM s (a, Criteria)
runRubric m = do
  (a, criteria) <- runWriterT m
  let check = left (intercalate ".") $ sanity criteria
  case check of
    Right _ -> return (a, criteria)
    Left path -> runIO . fail $ "Sum of weight in child rubrics was not 1 for '" <> path <> "'"

-- | Run the rubric monad, but get only the criteria
evalRubric :: RubricM s a -> SpecM s Criteria
evalRubric = fmap snd . runRubric

-- | Set up a criterion in the rubric. Like HSpec `describe` but with points.
criterion :: HasCallStack => String -> Float -> RubricM s () -> RubricM s ()
criterion name weight = bcriterion name weight 0

-- | Set up a criterion with an additional baseline.
bcriterion :: HasCallStack => String -> Float -> Float -> RubricM s () -> RubricM s ()
bcriterion name weight base body = do
  let runIO' = lift . runIO
  ref <- runIO' . newIORef $ []
  lift . describe name $ do
    (_, criteria) <- runWriterT body
    runIO $ writeIORef ref criteria
  criteria <- runIO' . readIORef $ ref
  tell [Criterion name weight base criteria]

-- | A shorthand for a criterion where the points will be
-- distributed later on by `distribute`. (this will award
-- NaN points)
dcriterion :: HasCallStack => String -> RubricM s () -> RubricM s ()
dcriterion = flip criterion (0/0)

-- | A test that awards points. Like HSpec `it` but with points.
passes :: (HasCallStack, Example s) => String -> Float -> s -> RubricM (Arg s) ()
passes n w = bpasses n w 0

-- | Set up a test with an additional baseline.
bpasses :: (HasCallStack, Example s) => String -> Float -> Float -> s -> RubricM (Arg s) ()
bpasses name weight base body = do
  tell [Criterion name weight base []]
  lift $ it name body

dpasses :: (HasCallStack, Example s) => String -> s -> RubricM (Arg s) ()
dpasses = flip passes (0/0)

-- | Distribute points among the current level of items.
distribute :: RubricM s a -> RubricM s a
distribute body = do
  let body' = runWriterT body
  (a, criteria) <- lift body'
  let weight = 1 / (fromIntegral . length) criteria
  let criteria' = (\r -> r { weight = weight }) <$> criteria
  tell criteria'
  return a

-- | A combinator that transforms an HSpec SpecWith into a pass-or-fail rubric.
-- So this awards full points iff all tests passed.
passOrFail :: SpecWith s -> RubricM s ()
passOrFail = lift
