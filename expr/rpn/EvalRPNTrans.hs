module EvalRPNTrans where

import Control.Applicative ( Alternative(empty) )
import Control.Monad.State
    ( guard,
      when,
      MonadTrans(lift),
      StateT,
      modify,
      MonadState(get, put),
      evalStateT, gets )
import Data.Foldable (traverse_)
import Text.Read (readMaybe)

type Stack = [Integer]
type EvalM = StateT Stack Maybe

push :: Integer -> EvalM ()
push x = modify (x:)

pop' :: EvalM Integer
pop' = do
  xs <- get
  when (null xs) $ lift Nothing
  put (tail xs)
  pure (head xs)

pop'' :: EvalM Integer
pop'' = do
  xs <- get
  guard (not $ null xs)
  put (tail xs)
  pure (head xs)

pop :: EvalM Integer
pop = do
  (x:xs) <- get
  put xs
  pure x

oneElementOnStack :: EvalM ()
oneElementOnStack = do
  l <- gets length
  guard (l == 1)

readSafe :: (Read a, Alternative m) => String -> m a
readSafe str =
  case readMaybe str of
    Nothing -> empty
    Just n -> pure n

evalRPN :: String -> Maybe Integer
evalRPN str = evalStateT evalRPN' []
  where
    evalRPN' = traverse_ step (words str) >> oneElementOnStack >> pop
    step "+" = processTops (+)
    step "*" = processTops (*)
    step "-" = processTops (-)
    step t   = readSafe t >>= push
    processTops op = flip op <$> pop <*> pop >>= push
