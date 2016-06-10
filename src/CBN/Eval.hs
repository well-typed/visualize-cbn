module CBN.Eval (
    Error
  , Step(..)
  , step
  ) where

import CBN.Language
import CBN.Heap
import CBN.Subst

type Error = String

data Step =
    -- | Evaluation took a single stpe
    Step (Heap Term) Term

    -- | We have reached weak head normal form
  | WHNF Value

    -- | The evaluator got stuck
  | Stuck Error

-- | Single execution step (small step semantics)
step :: Heap Term -> Term -> Step
step _  (TVar (Var x)) = Stuck $ "free variable " ++ show x
step _  (TLam x e)     = WHNF $ VLam x e
step _  (TCon c es)    = WHNF $ VCon c es
step hp (TPtr ptr) =
    case step hp $ deref (hp, ptr) of
      Step hp' e' -> Step (mutate (hp', ptr) e') (TPtr ptr)
      Stuck err   -> Stuck err
      WHNF val    -> WHNF val
step hp (TApp e1 e2) =
    case step hp e1 of
      Step hp' e1'      -> Step hp' $ TApp e1' e2
      Stuck err         -> Stuck err
      WHNF (VCon c _)   -> Stuck $ "Cannot apply " ++ c
      WHNF (VLam x e1') -> uncurry Step $ allocSubst [(x,e2)] (hp, e1')
step hp (TPat e ms) =
    case step hp e of
      Step hp' e'      -> Step hp' $ TPat e' ms
      Stuck err        -> Stuck err
      WHNF (VLam _ _)  -> Stuck "cannot pattern match on lambda"
      WHNF (VCon c es) ->
        case findMatch c ms of
          Nothing -> Stuck "Non-exhaustive pattern match"
          Just (xs, e') ->
            if length xs == length es
              then uncurry Step $ allocSubst (zip xs es) (hp, e')
              else Stuck "Invalid pattern match"

findMatch :: Con -> [Match] -> Maybe ([Var], Term)
findMatch c = go
  where
    go :: [Match] -> Maybe ([Var], Term)
    go []                                   = Nothing
    go (Match (Pat c' xs) e:ms) | c == c'   = Just (xs, e)
                                | otherwise = go ms
