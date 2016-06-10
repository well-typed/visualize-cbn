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
step _  (TPrim p [])   = WHNF $ VPrim p
step hp (TPtr ptr) =
    case step hp $ deref (hp, ptr) of
      Step hp' e' -> Step (mutate (hp', ptr) e') (TPtr ptr)
      Stuck err   -> Stuck err
      WHNF val    -> WHNF val
step hp (TApp e1 e2) =
    case step hp e1 of
      Step hp' e1'          -> Step hp' $ TApp e1' e2
      Stuck err             -> Stuck err
      WHNF (VCon (Con c) _) -> Stuck $ "Cannot apply " ++ show c
      WHNF (VLam x e1')     -> uncurry Step $ allocSubst [(x,e2)] (hp, e1')
      WHNF (VPrim _)        -> Stuck $ "Cannot apply primitive function"
step hp (TPat e ms) =
    case step hp e of
      Step hp' e'      -> Step hp' $ TPat e' ms
      Stuck err        -> Stuck err
      WHNF (VLam _ _)  -> Stuck "cannot pattern match on lambda"
      WHNF (VPrim _)   -> Stuck "cannot pattern match on primitive values"
      WHNF (VCon c es) ->
        case findMatch c ms of
          Nothing -> Stuck "Non-exhaustive pattern match"
          Just (xs, e') ->
            if length xs == length es
              then uncurry Step $ allocSubst (zip xs es) (hp, e')
              else Stuck "Invalid pattern match"
step hp (TPrim p es) =
    case stepPrimArgs hp es of
      PrimStep hp' es' -> Step hp' (TPrim p es')
      PrimValues vs    -> case delta p vs of
                            Left err -> Stuck err
                            Right p' -> Step hp (TPrim p' [])
      PrimStuck err    -> Stuck err

-- | The result of stepping the arguments to an n-ary primitive function
data StepPrimArgs =
    -- Some term took a step
    PrimStep (Heap Term) [Term]

    -- All terms were already in WHNF
  | PrimValues [Prim]

    -- A term tried to take a step but got stuck
  | PrimStuck Error

-- | Step the first argument that can step
stepPrimArgs :: Heap Term -> [Term] -> StepPrimArgs
stepPrimArgs hp = go []
  where
    go :: [Prim] -> [Term] -> StepPrimArgs
    go acc []     = PrimValues acc
    go acc (e:es) =
      case step hp e of
        WHNF (VPrim p) -> go (p:acc) es
        WHNF _         -> PrimStuck "Invalid argument to primitive function"
        Stuck err      -> PrimStuck err
        Step hp' e'    -> PrimStep hp' (acc' ++ [e'] ++ es)
          where
            acc' = map (valueToTerm . VPrim) (reverse acc)

findMatch :: Con -> [Match] -> Maybe ([Var], Term)
findMatch c = go
  where
    go :: [Match] -> Maybe ([Var], Term)
    go []                                   = Nothing
    go (Match (Pat c' xs) e:ms) | c == c'   = Just (xs, e)
                                | otherwise = go ms

delta :: Prim -> [Prim] -> Either Error Prim
delta PAdd [PInt n1, PInt n2] = Right $ PInt (n1 + n2)
delta PAdd     _ = Left "PAdd: invalid arguments"
delta (PInt _) _ = Left "PInt: cannot apply"
