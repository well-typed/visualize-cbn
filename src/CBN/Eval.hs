module CBN.Eval (
    Error
  , Description(..)
  , DescriptionWithContext(..)
  , Step(..)
  , step
  ) where

import CBN.Language
import CBN.Heap
import CBN.Subst
import qualified Data.Map as M

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
  | StepDelta PrimApp

    -- | Pattern-match
  | StepMatch Con

    -- | Evaluated conditional
  | StepIf Bool

    -- | Seq finished evaluating its left argument
  | StepSeq
  deriving (Show)

data DescriptionWithContext =
    DescriptionWithContext Description [Ptr]
  deriving (Show)

data Step =
    -- | Evaluation took a single step
    Step DescriptionWithContext (Heap Term, Term)

    -- | We have reached weak head normal form
  | WHNF Value

    -- | The evaluator got stuck
  | Stuck Error
  deriving (Show)

emptyContext :: Description -> (Heap Term, Term) -> Step
emptyContext descr =
    Step (DescriptionWithContext descr [])

pushContext :: Ptr -> DescriptionWithContext -> (Heap Term, Term) -> Step
pushContext ptr (DescriptionWithContext descr context) =
    Step (DescriptionWithContext descr (ptr:context))

-- | Single execution step (small step semantics)
step :: (Heap Term, Term) -> Step
step (_, TVar (Var x))         = Stuck $ "free variable " ++ show x
step (_, TLam x e)             = WHNF $ VLam x e
step (_, TCon ces)             = WHNF $ VCon ces
step (_, TPrim (PrimApp p [])) = WHNF $ VPrim p
step (hp, TPtr ptr) =
    case deref (hp, ptr) of
      Nothing -> let ptrs = M.keys $ heapEntries hp
                 in
                   Stuck $ "Invalid reference to symbol "
                   ++ pprintPtr ptr
                   ++ ". Valid symbol names are: "
                   ++ show (map pprintPtr ptrs)
      Just p  ->
        case step (hp, p) of
          Step d (hp', e') -> pushContext ptr d (mutate (hp', ptr) e', TPtr ptr)
          Stuck err        -> Stuck err
          WHNF val         -> WHNF val
step (hp, TLet x e1 (TSeq (TVar x') e2)) | x == x' =
    -- special case for let x = e in seq x ..
    -- rather than allocate we reduce inside the let
    case step (hp, e1) of
      Step d (hp', e1') -> Step d (hp', TLet x e1' (TSeq (TVar x) e2))
      Stuck err         -> Stuck err
      WHNF _            -> emptyContext StepSeq (hp, TLet x e1 e2)
step (hp, TLet x e1 e2) =
    emptyContext StepAlloc $ allocSubst RecBinding [(x,e1)] (hp, e2)
step (hp, TApp e1 e2) = do
    let descr = case e1 of
                  TPtr ptr   -> StepApply ptr
                  _otherwise -> StepBeta
    case step (hp, e1) of
      Step d (hp', e1') -> Step d (hp', TApp e1' e2)
      Stuck err         -> Stuck err
      WHNF (VLam x e1') -> emptyContext descr $ allocSubst NonRecBinding [(x,e2)] (hp, e1')
      WHNF _            -> Stuck "expected lambda"
step (hp, TCase e ms) =
    case step (hp, e) of
      Step d (hp', e') -> Step d (hp', TCase e' ms)
      Stuck err        -> Stuck err
      WHNF (VLam _ _)  -> Stuck "cannot pattern match on lambda"
      WHNF (VPrim _)   -> Stuck "cannot pattern match on primitive values"
      WHNF (VCon (ConApp c es)) ->
        case findMatch c ms of
          Nothing -> Stuck "Non-exhaustive pattern match"
          Just (xs, e') ->
            if length xs == length es
              then emptyContext (StepMatch c) $ allocSubst NonRecBinding (zip xs es) (hp, e')
              else Stuck $ "Invalid pattern match (cannot match " ++ show (xs, es) ++ ")"
step (hp, TPrim (PrimApp p es)) =
    case stepPrimArgs hp es of
      PrimStep d hp' es' -> Step d (hp', TPrim (PrimApp p es'))
      PrimWHNF vs        -> let descr = StepDelta (PrimApp p (map (valueToTerm . VPrim) vs))
                            in case delta p vs of
                              Left err -> Stuck err
                              Right e' -> emptyContext descr (hp, valueToTerm e')
      PrimStuck err      -> Stuck err
step (hp, TIf c t f) =
    case step (hp, c) of
      Step d (hp', c') -> Step d (hp', TIf c' t f)
      Stuck err        -> Stuck err
      WHNF val | val == liftBool True  -> emptyContext (StepIf True)  (hp, t)
               | val == liftBool False -> emptyContext (StepIf False) (hp, f)
               | otherwise             -> Stuck "expected bool"
step (hp, TSeq e1 e2) =
    case step (hp, e1) of
      Step d (hp', e1') -> Step d (hp', TSeq e1' e2)
      Stuck err         -> Stuck err
      WHNF _            -> emptyContext StepSeq (hp, e2)

-- | The result of stepping the arguments to an n-ary primitive function
data StepPrimArgs =
    -- Some term took a step
    PrimStep DescriptionWithContext (Heap Term) [Term]

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

delta :: Prim -> [Prim] -> Either Error Value
delta PIAdd [PInt n1, PInt n2] = Right $ liftInt  $ n1 +  n2
delta PISub [PInt n1, PInt n2] = Right $ liftInt  $ n1 -  n2
delta PIMul [PInt n1, PInt n2] = Right $ liftInt  $ n1 *  n2
delta PIEq  [PInt n1, PInt n2] = Right $ liftBool $ n1 == n2
delta PILt  [PInt n1, PInt n2] = Right $ liftBool $ n1 <  n2
delta PILe  [PInt n1, PInt n2] = Right $ liftBool $ n1 <= n2
delta _op _args = Left $ "delta: cannot evaluate"
