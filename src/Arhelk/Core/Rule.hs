module Arhelk.Core.Rule(
    Rule
  , RuleM
  , propose
  , imply
  , implyNothing
  , implyMap
  , runRule
  ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Lens.Simple

newtype RuleM a b c = RuleM { unRuleM :: ReaderT a (WriterT b Identity) c }
  deriving (Functor, Applicative, Monad, MonadReader a, MonadWriter b)

type Rule a = RuleM a [a] ()

propose :: Setter a a' b (Maybe b') -> b' -> RuleM r [a] c -> RuleM r [a'] c
propose field v (RuleM subrule) = do
  r <- ask 
  let Identity (c, ws) = runWriterT (runReaderT subrule r)
  tell $ set field (Just v) <$> ws 
  return c

imply :: Setter a a' b (Maybe b') -> b' -> RuleM a [a'] ()
imply field v = do 
  a <- ask
  tell [set field (Just v) a]

implyNothing :: RuleM a [a] ()
implyNothing = do
  w <- ask
  tell [w]

implyMap :: (a -> [a']) -> RuleM r [a] c -> RuleM r [a'] c
implyMap f (RuleM rule) = do
  r <- ask 
  let Identity (c, ws) = runWriterT (runReaderT rule r)
  tell $ concat $ f <$> ws
  return c 

runRule :: Monoid a => Rule a -> [a]
runRule = snd . runIdentity . runWriterT . flip runReaderT mempty . unRuleM