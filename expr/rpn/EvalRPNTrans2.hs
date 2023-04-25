module EvalRPNTrans2 where

import Control.Monad.State ( guard, modify, MonadState(get, put), evalState, State, gets )
import Control.Applicative ( Alternative(empty) )
import Data.Foldable (traverse_)
import Text.Read (readMaybe)

import MyMaybeT ( MaybeT(runMaybeT) )

type Stack = [Integer]
type EvalM = MaybeT (State Stack)

push :: Integer -> EvalM ()
push x = modify (x:)

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
evalRPN str = evalState (runMaybeT evalRPN') []
  where
    evalRPN' = traverse_ step (words str) >> oneElementOnStack >> pop
    step "+" = processTops (+)
    step "*" = processTops (*)
    step "-" = processTops (-)
    step t   = readSafe t >>= push
    processTops op = flip op <$> pop <*> pop >>= push
