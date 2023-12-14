module CBN.Eval (
    Error
  , Description(..)
  , DescriptionWithContext(..)
  , Step(..)
  , step
    -- * Case statements
  , findMatch
  , AllocdConArgs(..)
  , allocConArgs
  ) where

import qualified Data.Map as M

import CBN.Language
import CBN.Heap
import CBN.Subst

{-------------------------------------------------------------------------------
  Small-step semantics
-------------------------------------------------------------------------------}

type Error = String

-- | Description of a step: what happened?
data Description =
    -- | We moved let-bound variables to the heap
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

    -- | We allocated constructor arguments to preserve sharing
  | StepAllocConArgs
  deriving (Show)

data DescriptionWithContext =
    DescriptionWithContext Description Context
  deriving (Show)

type Context = [Ptr]

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
step (hp, TLet bound e) =
    emptyContext StepAlloc $ allocSubst bound (hp, e)
step (hp, TApp e1 e2) = do
    let descr = case e1 of
                  TPtr ptr   -> StepApply ptr
                  _otherwise -> StepBeta
    case step (hp, e1) of
      Step d (hp', e1') -> Step d (hp', TApp e1' e2)
      Stuck err         -> Stuck err
      WHNF (VLam x e1') -> emptyContext descr $ allocSubst [(x,e2)] (hp, e1')
      WHNF _            -> Stuck "expected lambda"
step (hp, TCase e ms) =
    case step (hp, e) of
      Step d (hp', e') -> Step d (hp', TCase e' ms)
      Stuck err        -> Stuck err
      WHNF (VLam _ _)  -> Stuck "cannot pattern match on lambda"
      WHNF (VPrim _)   -> Stuck "cannot pattern match on primitive values"
      WHNF (VCon (ConApp c es)) ->
        case findMatch c ms of
          Nothing ->
            Stuck "Non-exhaustive pattern match"
          Just (xs, _) | length xs /= length es ->
            Stuck $ "Cannot match " ++ show (xs, es)
          Just (xs, rhs) ->
            -- We /know/ that e is a con-app or a pointer to a con-app, but we
            -- search /again/, this time with 'allocConArgs'. The reason we
            -- search twice is that the first search enables us to find the
            -- right variable names to use for allocation. This is not critical,
            -- but makes the variables in the heap more human-friendly.
            case allocConArgs xs (hp, e) of
              ConArgsAllocFailed ->
                error "step: impossible ConArgsAllocFailed"
              ConArgsAllocUnnecessary _ ->
                emptyContext (StepMatch c) $ allocSubst (zip xs es) (hp, rhs)
              ConArgsAllocDone (ctxt, hp', e') _ ->
                Step (DescriptionWithContext StepAllocConArgs ctxt) (hp', TCase e' ms)
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

{-------------------------------------------------------------------------------
  Case statements
-------------------------------------------------------------------------------}

findMatch :: Con -> Branches -> Maybe ([Var], Term)
findMatch c (Matches ms) = go ms
  where
    go :: [Match] -> Maybe ([Var], Term)
    go []                                    = Nothing
    go (Match (Pat c' xs) e:ms') | c == c'   = Just (xs, e)
                                 | otherwise = go ms'
findMatch c (Selector s) =
    findMatch c $ Matches [selectorMatch s]

data AllocdConArgs =
    -- | No allocation was necessary
    ConArgsAllocUnnecessary ConApp

    -- | The constructor arguments were heap-allocated
  | ConArgsAllocDone (Context, Heap Term, Term) ConApp

    -- | The term was not a constructor application in WHNF or a pointer to
    -- such a term
  | ConArgsAllocFailed

-- | Allocate constructor arguments
--
-- This is necessary when doing a case statement on a value in the heap, to
-- avoid losing sharing.
allocConArgs ::
     [Var]
  -> (Heap Term, Term)
  -> AllocdConArgs
allocConArgs xs =
    go True
  where
    go :: Bool -> (Heap Term, Term) -> AllocdConArgs
    go isTopLevel (hp, term) =
        case term of
          TPtr ptr | Just p <- deref (hp, ptr) -> do
            case go False (hp, p) of
              ConArgsAllocUnnecessary conApp ->
                ConArgsAllocUnnecessary conApp
              ConArgsAllocFailed ->
                ConArgsAllocFailed
              ConArgsAllocDone (ctxt, hp', e') conApp ->
                ConArgsAllocDone
                  (ptr : ctxt, mutate (hp', ptr) e', TPtr ptr)
                  conApp
          TCon conApp@(ConApp con args) | length args == length xs ->
            if isTopLevel || all termIsSimple args then
              ConArgsAllocUnnecessary conApp
            else do
              let (hp', args') =
                     allocMany
                       (zipWith prepareHeapEntry xs args)
                       processHeapEntries
                       hp
                  conApp' = ConApp con args'
              ConArgsAllocDone ([], hp', TCon conApp') conApp'
          _ ->
            ConArgsAllocFailed

    prepareHeapEntry :: Var -> Term -> (Maybe String, Ptr -> (Ptr, Term))
    prepareHeapEntry x t = (Just (varName x), \ptr -> (ptr, t))

    processHeapEntries :: [(Ptr, Term)] -> ([(Ptr, Term)], [Term])
    processHeapEntries entries = (entries, map (TPtr . fst) entries)

{-------------------------------------------------------------------------------
  Primitive operations
-------------------------------------------------------------------------------}

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

delta :: Prim -> [Prim] -> Either Error Value
delta PISucc [PInt n]           = Right $ liftInt  $ n + 1
delta PIAdd  [PInt n1, PInt n2] = Right $ liftInt  $ n1 + n2
delta PISub  [PInt n1, PInt n2] = Right $ liftInt  $ n1 - n2
delta PIMul  [PInt n1, PInt n2] = Right $ liftInt  $ n1 * n2
delta PIMin  [PInt n1, PInt n2] = Right $ liftInt  $ n1 `min` n2
delta PIMax  [PInt n1, PInt n2] = Right $ liftInt  $ n1 `max` n2
delta PIEq   [PInt n1, PInt n2] = Right $ liftBool $ n1 == n2
delta PILt   [PInt n1, PInt n2] = Right $ liftBool $ n1 <  n2
delta PILe   [PInt n1, PInt n2] = Right $ liftBool $ n1 <= n2
delta _op _args = Left $ "delta: cannot evaluate"

