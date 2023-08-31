module Test.Hrubric
  ( Criteria
  , Criterion (..)
  , grade

  , RubricM (..)
  , runRubricM
  , evalRubricM

  , Rubric
  , hrubric
  , criterion
  , passes
  , dcriterion
  , dpasses
  , distribute
  , passOrFail
  ) where

import Test.Hspec
import Test.Hspec.Runner hiding (Path)
import System.Environment

import Data.List (foldl', isPrefixOf, intercalate)

import Control.Monad.Writer
import Control.Monad.Except
import Control.Arrow

-- | The list of criteria in a rubric.
-- The final weigth of the criteria
-- should add up to 1.0 (or just be
-- distribute for all instances)
type Criteria = [Criterion]

-- | A criterion in a rubric. Contains
-- nested rubrics in a tree to place
-- weights on sub-criteria
data Criterion = Criterion
  { name   :: String
  , weight :: Float
  -- Empty rubric means pass or fail
  , nodes  :: Criteria
  }
  deriving (Show, Eq)

-- A path to a test
type Path = [String]

-- | Award a grade for the results given a rubric
grade :: Criteria -> SpecResult -> Float
grade criteria result = check (expand criteria) (specResultItems result)
  where
    -- Check all paths of the rubric against the results
    check :: [(Float, Path)] -> [ResultItem] -> Float
    check rs is = foldl' (\g r -> g + award r is) 0 rs

    -- Award points if the result items that match all passed
    award :: (Float, Path) -> [ResultItem] -> Float
    award (g, p) is = do
      let matches = filter (subpath p) is
      -- No points if there is a failure.
      -- A test case wasn't run if there were no matches,
      -- so we don't award points in that case.
      if null matches || any resultItemIsFailure matches
        then 0
        else g

    -- Check if a Path path matches the item path
    subpath :: Path -> ResultItem -> Bool
    subpath p q = do
      let (q', n) = resultItemPath q
      isPrefixOf p (q' ++ [n])

    -- Expand criteria to a list of all tests paths and their weights
    expand :: Criteria -> [(Float, Path)]
    expand = expand' (1.0, [])
      where
        expand' :: (Float, Path) -> Criteria -> [(Float, Path)]
        expand' tup []        = [tup]
        expand' (g, p) (c:[]) = recurse g p c
        expand' (g, p) (c:cs) = recurse g p c ++ expand' (g, p) cs

        -- calculate the weigth, append name and recursively expand paths
        recurse g p c = expand' (g * weight c, p ++ [name c]) (nodes c)

-- | Do a sanity check on the criteria, returning
-- a path to the set of criteria that do not match
-- if this exists.
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

-- | Check sanity (i.e. total weight of 1) of one
-- layer in the tree (so no recursive check)
-- Empty tree is always sane, as it is treated
-- as a pass or fail.
sane :: Criteria -> Bool
sane [] = True
sane c  = all inRange weights && cmp total 1
  where
    inRange f = 0 <= f && f <= 1

    total = sum weights
    weights = map weight c

    -- Adjust for floating point errors
    cmp x y = abs (x-y) < 0.0001

-- | We cannot stack monads on SpecM, because we cannot
-- call runSpecM on a SpecM Rubric (only on SpecM ()
-- which means there is no way to get the rubric out
-- of the WriterT monad when SpecM is stacked into
-- it...)
--
-- Hence, we have this cursed Monad instead.
data RubricM s a = RubricM (Writer Criteria a) (SpecWith s)

type Rubric = RubricM () ()

instance Functor (RubricM s) where
  fmap f (RubricM w s) = RubricM (f <$> w) s

instance Applicative (RubricM a) where
  pure x = RubricM (pure x) (pure ())
  (<*>) = undefined

instance Monad (RubricM a) where
  RubricM r s >>= f = RubricM r'' s''
    where
      ~(a, w ) = runWriter r
      RubricM r' s' = f a
      ~(b, w') = runWriter $ r'
   
      r'' = writer (b, w <> w')
      s''  = s >> s'

-- | Run the full rubric, this can be compared
-- to the hspec function.
hrubric :: Rubric -> IO (Either String Float)
hrubric rubric = runExceptT $ do
  args <- liftIO getArgs
  cfg <- liftIO $ readConfig defaultConfig args
  (criteria, spec) <- liftEither $ evalRubricM rubric
  (cfg', forest) <- liftIO $ evalSpec cfg spec
  result <- liftIO . withArgs [] $ runSpecForest forest cfg'
  return $ grade criteria result

-- | Run the rubric monad
runRubricM :: RubricM s a -> Either String ((a, Criteria), SpecWith s)
runRubricM (RubricM c s) = do
  let (a, c') = runWriter c
  left (intercalate ".") $ sanity c'
  return ((a, c'), s)

-- | Run the rubric monad, but get only the criteria
evalRubricM :: RubricM s a -> Either String (Criteria, SpecWith s)
evalRubricM rubric = do
  ((_, c), s) <- runRubricM rubric
  return (c, s)

-- | Set up a criterion in the rubric. Like HSpec `describe` but with points.
criterion :: HasCallStack => String -> Float -> RubricM s a -> RubricM s a
criterion n w (RubricM c s) = RubricM c' (describe n s)
  where
    (a, criteria) = runWriter c
    c' = writer (a, [Criterion n w criteria])

-- | A shorthand for a criterion where the points will be
-- distributed later on by `distribute`. (this will award
-- NaN points)
dcriterion :: HasCallStack => String -> RubricM s a -> RubricM s a
dcriterion = flip criterion (0/0)

-- | A test that awards points. Like HSpec `it` but with points.
passes :: (HasCallStack, Example s) => String -> Float -> s -> RubricM (Arg s) ()
passes n w s = RubricM (writer ((), [crit])) (it n s)
  where
    crit = Criterion n w []

dpasses :: (HasCallStack, Example s) => String -> s -> RubricM (Arg s) ()
dpasses = flip passes (0/0)

-- | Distribute points among the current level of items.
distribute :: RubricM s a -> RubricM s a
distribute (RubricM r s) = RubricM (writer (a, criteria')) s
  where
    (a, criteria) = runWriter r
    w = 1 / (fromIntegral . length) criteria
    criteria' = map (\r' -> r' { weight = w }) criteria

-- | A combinator that transforms an HSpec SpecWith into a pass-or-fail rubric.
-- So this awards full points iff all tests passed.
passOrFail :: SpecWith s -> RubricM s ()
passOrFail = RubricM (writer ((), []))
