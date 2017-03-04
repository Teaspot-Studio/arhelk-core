module Arhelk.Core.Rule(
    Rule
  , RuleM
  , proposeMany
  , propose
  , imply
  , implyNothing
  , implyMap
  , runRule
  ) where

import Control.Monad.Reader
import Control.Monad.Trans.RSS.Strict
import Control.Monad.Writer
import Lens.Simple

newtype RuleM a b c = RuleM { unRuleM :: RSST a b () Identity c }
  deriving (Functor, Applicative, Monad, MonadReader a, MonadWriter b)

type Rule a = RuleM a [a] ()

-- | Proposing a hypothesis to following actions. If action generates some
-- hypothesis, the function modifies the values with given setter and value.
--
-- Examples:
--
-- >>> propose fieldA valueA $ imply fieldB valueB
-- [{ fieldA = valueA, fieldB = valueB }]
--
-- >>> propose fieldA valueA (imply fieldB valueB1 >> imply fieldB valueB2)
-- [{ fieldA = valueA, fieldB = valueB1 }, { fieldA = valueA, fieldB = valueB2 }]
propose :: Setter a a' b (Maybe b') -> b' -> RuleM r [a] c -> RuleM r [a'] c
propose field v = proposeMany field [v]

-- | Proposing a multiple hypothesis to following actions. If action generates some
-- hypothesis, the function modifies the values with given setter and values.
--
-- Examples:
--
-- >>> proposeMany fieldA [valueA1, valueA2] $ imply fieldB valueB
-- [{ fieldA = Just valueA1, fieldB = Just valueB }, { fieldA = Just valueA2, fieldB = Just valueB }]
--
-- >>> propose fieldA [valueA1, valueA2] (imply fieldB valueB1 >> imply fieldB valueB2)
-- [{ fieldA = Just valueA1, fieldB = Just valueB1 }, { fieldA = Just valueA1, fieldB = Just valueB2 }, { fieldA = Just valueA2, fieldB = Just valueB1 }, { fieldA = Just valueA2, fieldB = Just valueB2 }]
proposeMany :: Setter a a' b (Maybe b') -> [b'] -> RuleM r [a] c -> RuleM r [a'] c
proposeMany field vs (RuleM subrule) = do
  r <- ask
  let Identity (c, _, ws) = runRSST subrule r ()
  tell $ concat $ (\v -> set field (Just v) <$> ws) <$> vs
  return c

-- | Generates new hypothesis by modifying base value with given setter and value.
--
-- >>> imply fieldA valueA
-- { fieldA = Just valueA, fieldB = Nothing, ... }
imply :: Setter a a' b (Maybe b') -> b' -> RuleM a [a'] ()
imply field v = do
  a <- ask
  tell [set field (Just v) a]

-- | Generates new hypothesis by placing base value into set of hypothesises.
--
-- >>> implyNothing
-- { fieldA = Nothing, fieldB = Nothing, ... }
implyNothing :: RuleM a [a] ()
implyNothing = do
  w <- ask
  tell [w]

-- | Transforms all hypothesises that are produced by given function
--
-- @implyMap f rule@ will apply __f__ to each hypothesis produced by __rule__
implyMap :: (a -> [a']) -> RuleM r [a] c -> RuleM r [a'] c
implyMap f (RuleM rule) = do
  r <- ask
  let Identity (c, _, ws) = runRSST rule r ()
  tell $ concat $ f <$> ws
  return c

-- | Runs rule and returns list of all produced hypothesises
runRule :: Monoid a => Rule a -> [a]
runRule = (\(_, _, a) -> a) . runIdentity . (\ m -> runRSST m mempty ()) . unRuleM
