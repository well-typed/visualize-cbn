module CBN.Eval (
    Error
  , Description(..)
  , Step(..)
  , step
  ) where

import CBN.Language
import CBN.Heap
import CBN.Subst

type Error = String

-- | Description of a step: what happened?
data Description =
    -- | We moved a let-bound variable to the heap
    StepAlloc

    -- | Beta-reduction
  | StepBeta

    -- | Like beta-reduction, but apply a named function
  | StepApply Ptr

    -- | Delta-reduction
  | StepDelta Prim

    -- | Pattern-match
  | StepMatch

data Step =
    -- | Evaluation took a single stpe
    Step Description (Heap Term, Term)

    -- | We have reached weak head normal form
  | WHNF Value

    -- | The evaluator got stuck
  | Stuck Error

-- | Single execution step (small step semantics)
step :: (Heap Term, Term) -> Step
step (_, TVar (Var x)) = Stuck $ "free variable " ++ show x
step (_, TLam x e)     = WHNF $ VLam x e
step (_, TCon c es)    = WHNF $ VCon c es
step (_, TPrim p [])   = WHNF $ VPrim p
step (hp, TPtr ptr) =
    case step (hp, deref (hp, ptr)) of
      Step d (hp', e') -> Step d (mutate (hp', ptr) e', TPtr ptr)
      Stuck err        -> Stuck err
      WHNF val         -> WHNF val
step (hp, TLet x e1 e2) =
    Step StepAlloc $ allocSubst RecBinding [(x,e1)] (hp, e2)
step (hp, TApp e1 e2) = do
    let descr = case e1 of
                  TPtr ptr   -> StepApply ptr
                  _otherwise -> StepBeta
    case step (hp, e1) of
      Step d (hp', e1')     -> Step d (hp', TApp e1' e2)
      Stuck err             -> Stuck err
      WHNF (VCon (Con c) _) -> Stuck $ "Cannot apply " ++ show c
      WHNF (VLam x e1')     -> Step descr $ allocSubst NonRecBinding [(x,e2)] (hp, e1')
      WHNF (VPrim _)        -> Stuck $ "Cannot apply primitive function"
step (hp, TCase e ms) =
    case step (hp, e) of
      Step d (hp', e') -> Step d (hp', TCase e' ms)
      Stuck err        -> Stuck err
      WHNF (VLam _ _)  -> Stuck "cannot pattern match on lambda"
      WHNF (VPrim _)   -> Stuck "cannot pattern match on primitive values"
      WHNF (VCon c es) ->
        case findMatch c ms of
          Nothing -> Stuck "Non-exhaustive pattern match"
          Just (xs, e') ->
            if length xs == length es
              then Step StepMatch $ allocSubst NonRecBinding (zip xs es) (hp, e')
              else Stuck $ "Invalid pattern match (cannot match " ++ show (xs, es) ++ ")"
step (hp, TPrim p es) =
    case stepPrimArgs hp es of
      PrimStep d hp' es' -> Step d (hp', TPrim p es')
      PrimWHNF vs        -> case delta p vs of
                              Left err -> Stuck err
                              Right e' -> Step (StepDelta p) (hp, e')
      PrimStuck err      -> Stuck err

-- | The result of stepping the arguments to an n-ary primitive function
data StepPrimArgs =
    -- Some term took a step
    PrimStep Description (Heap Term) [Term]

    -- All terms were already in WHNF
  | PrimWHNF [Prim]

    -- A term tried to take a step but got stuck
  | PrimStuck Error

-- | Step the first argument that can step
stepPrimArgs :: Heap Term -> [Term] -> StepPrimArgs
stepPrimArgs hp = go []
  where
    go :: [Prim] -> [Term] -> StepPrimArgs
    go acc []     = PrimWHNF (reverse acc)
    go acc (e:es) =
      case step (hp, e) of
        WHNF (VPrim p)   -> go (p:acc) es
        WHNF _           -> PrimStuck "Invalid argument to primitive function"
        Stuck err        -> PrimStuck err
        Step d (hp', e') -> PrimStep d hp' (acc' ++ [e'] ++ es)
          where
            acc' = map (valueToTerm . VPrim) (reverse acc)

findMatch :: Con -> [Match] -> Maybe ([Var], Term)
findMatch c = go
  where
    go :: [Match] -> Maybe ([Var], Term)
    go []                                   = Nothing
    go (Match (Pat c' xs) e:ms) | c == c'   = Just (xs, e)
                                | otherwise = go ms

delta :: Prim -> [Prim] -> Either Error Term
delta PIAdd [PInt n1, PInt n2] = Right $ liftInt  $ n1 +  n2
delta PIEq  [PInt n1, PInt n2] = Right $ liftBool $ n1 == n2
delta PILt  [PInt n1, PInt n2] = Right $ liftBool $ n1 <  n2
delta PILe  [PInt n1, PInt n2] = Right $ liftBool $ n1 <= n2
delta _op _args = Left $ "delta: cannot evaluate"

{-------------------------------------------------------------------------------
  Lifting from Haskell to our object language
-------------------------------------------------------------------------------}

liftInt :: Integer -> Term
liftInt n = TPrim (PInt n) []

liftBool :: Bool -> Term
liftBool True  = TCon (Con "True")  []
liftBool False = TCon (Con "False") []
