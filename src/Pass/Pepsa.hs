-- PEPSA stands for Partially Evaluating Partially Static Applications (of
-- rank-2 polymorphic functions)
module Pass.Pepsa (pepsa) where

import Control.Monad
import Data.List (nub)
import Engines.BetaReduction (betaReduceCompletelyM)
import Engines.LeftElaboration
import Engines.Transform (FullTransform (fullTransformM))
import GHC.Core.Map.Type (deBruijnize)
import GHC.Plugins
import Utils

-- | The memoized specializing pass specializes traversals.
pepsa :: Opts -> CoreToDo
pepsa opts = CoreDoPluginPass "PEPSA" (pepsaModGuts opts)

-------------------------------------------------------------------------------
--
-- The entry point of the plugin pass. The concept is simple: repeatedly
-- apply the transformation (iterateTransformation) until either:
--    1. There is nothing left to do
--    2. The number of iterations (provided by the user) has been exhausted.
-- Then, just return the transformed program.
--
-------------------------------------------------------------------------------
pepsaModGuts :: Opts -> CorePluginPass
pepsaModGuts opts mod_guts = do
    prtSIf (debug opts) (info "Partial Evaluator")
    let original_binds = mg_binds mod_guts
    new_binds <- iterateTransformation opts original_binds []
    prtSIf (debug opts) (info "Complete. Program before transformation:")
    prtIf (debug opts) original_binds
    prtSIf (debug opts) (info "Program after transformation:")
    prtIf (debug opts) new_binds
    return mod_guts{mg_binds = new_binds}

-- | The number of iterations to perform.
type NumIterations = Int

-------------------------------------------------------------------------------
--
-- This is the main function of the plugin that controls iteration and (more
-- importantly) what to do in each iteration.
--
-- In each iteration, the recipe is as follows:
--    1. Find the targets of optimization
--    2. Optimize a single target (if there are any). Note that only a SINGLE
--       target is optimized.
--    3. Iterate; stop when nothing is to be optimized or when the iteration
--       count reaches 0. Note that the number of iterations to perform is
--       controlled by the plugin options (num_iterations).
--
-------------------------------------------------------------------------------

{- | Repeatedly perform the transformation on the binds until no more targets
are found or the number of iterations reaches zero.
-}
iterateTransformation ::
    -- | Plugin options
    Opts ->
    -- | The program
    CoreProgram ->
    -- | The memoization table
    MemoTable ->
    -- | The transformed program
    CoreM CoreProgram
iterateTransformation opts = aux n
  where
    n = num_iterations opts
    aux :: NumIterations -> CoreProgram -> MemoTable -> CoreM CoreProgram
    aux 0 pgm _ = do
        prtSIf (debug opts) (warn $ "Fully exhausted all " ++ show n ++ " iterations")
        return pgm
    aux m pgm₁ memo₁ = do
        prtSIf (debug opts) (info $ "Iteration " ++ show (n - m + 1))
        -- Step 1 is to obtain all the targets of optimization
        -- via the rules. Thus, we first obtain the initial
        -- targets (initialTargets), then repeatedly recursively obtain the
        -- targets until a fixed point (recursivelyAcquireTargets).
        i_ <- getRank2Targets pgm₁ pgm₁
        targets <- recursivelyAcquireTargets i_ pgm₁
        -- Step 2 is to perform the actual optimization on the targets.
        (pgm₂, memo₂, flag, floated_binds) <- optimize pgm₁ targets pgm₁ memo₁
        if not flag
            then -- Nothing happened; it appears that we have optimized the program
            -- to a fixed point. Just return the resulting program.
                return pgm₂
            else do
                -- Stuff happened. Check if any new memoized bindings have been
                -- generated.
                let pgm₃ = case floated_binds of
                        -- If there is nothing, then no need to add new binds.
                        Nothing -> pgm₂
                        -- A bind was floated out; Add that bind to the top-level program.
                        Just (floated_id, floated_expr) ->
                            NonRec floated_id floated_expr : pgm₂
                -- Continue the transformation.
                aux (m - 1) pgm₃ memo₂

-------------------------------------------------------------------------------
--
-- Getting targets.
--
-- Targettable describes stuff from which targets can be obtained. The way
-- we get targets is by fixed point computation. The first method,
-- getRank2Targets, is our starting point---rank-2 polymorphic functions are
-- targets in the ith arguments (where the ith arguments are themselves
-- polymorphic). The recursivelyAcquireTargets function then takes those
-- targets from getRank2Targets and recursively finds other targets that, once
-- transformed, could reveal more optimization targets. The
-- recursivelyAcquireTargets function relies heavily on the getTargets method.
--
-- See Also:
--    1) typeRank2
--    2) dictArgIndex
--    3) boundIn
--    4) getAppViews
--    5) matchAppViewsToTargets
--
-------------------------------------------------------------------------------

{- | A list of targets, where each target is a pair of an 'Id' and a list
of 'Indices'.
-}
type Targets = [(Id, Indices)]

{- | Indices are **sorted** lists of ints. It is virtually always nonempty
unless used intermediately.
-}
type Indices = [Int]

-- | Inserts an Int into the Indices, keeping them sorted.
(@+) :: Indices -> Int -> Indices
[] @+ i = [i]
(x : xs) @+ i
    | i < x = i : x : xs
    | i == x = x : xs
    | otherwise = x : (xs @+ i)

-- Just takes the maximum index in some Indices, lol.
largestIndex :: Indices -> Int
largestIndex = foldr max (-1)

type InScopeBinds = [CoreBind]

boundIn :: Id -> InScopeBinds -> Bool
boundIn _ [] = False
boundIn v (NonRec v' _ : xs)
    | v == v' = True
    | otherwise = v `boundIn` xs
boundIn v (Rec ls : xs) = v `elem` map fst ls || v `boundIn` xs

type LHS = Id

type LambdaBoundVars = [Var]

class Targettable a where
    -- Obtains all targets based on being rank-2 polymorphic functions.
    getRank2Targets :: a -> InScopeBinds -> CoreM Targets

    -- The idea behind getTargets is that we want to find **binds** in the form
    -- of  my_fn = \x.\y...\z. e  such that e contains a target, and that with
    -- substituting x, y, ..., z etc for a partially static argument, the
    -- target also becomes partially static.
    --
    -- Therefore, while traversing into the program, we must keep track of
    -- any LHS Ids and RHS lambda-bound variables.
    getTargets ::
        a ->
        -- The enclosing LHS. This is a Just lhs if we are currently traversing
        -- something like lhs = \x.\y... . If we are not, this should be set to
        -- Nothing
        Maybe LHS ->
        -- This is the list of lambda-bound variables when traversing a lambda
        -- expression. This list should be emptied when we are no longer traversing
        -- a lambda expression.
        LambdaBoundVars ->
        -- The targets used to determine if a function should also be a target
        Targets ->
        -- binds that are in-scope
        InScopeBinds ->
        CoreM Targets

instance Targettable CoreExpr where
    getRank2Targets (Var v) binds = do
        -- We use the type of the var to determine if it is rank-2 polymorphic.
        let t = exprType (Var v)
        let indices = typeRank2 t
        -- Check if this is a rank-2 polymorphic class method, in which case,
        -- make sure the dictionary argument is part of the target indices.
        if not (null indices)
            then case idDetails v of
                -- Is a class method---add the dictionary argument as part of
                -- the target too!
                ClassOpId{} -> return [(v, indices @+ dictArgIndex t)]
                _ ->
                    -- There is no use setting something a target if it cannot be inlined
                    -- so only add something as a target if either there is a let binding
                    -- that defines it and is in scope, or if there is an unfolding
                    -- present.
                    if v `boundIn` binds
                        then return [(v, indices)]
                        else case realIdUnfolding v of
                            CoreUnfolding{} -> return [(v, indices)]
                            _ -> return []
            else return []
    -- For the other constructors, just propagate 'getRank2Targets' and combine
    -- the results.
    getRank2Targets (App f x) binds = do
        t1 <- getRank2Targets f binds
        t2 <- getRank2Targets x binds
        return $ t1 ++ t2
    getRank2Targets (Lam _ e) binds = getRank2Targets e binds
    getRank2Targets (Let b e) binds = do
        -- The bind b must be included in the recursive calls to getRank2Targets
        -- since they become in scope in the binds and the expression.
        t1 <- getRank2Targets e (b : binds)
        t2 <- getRank2Targets b (b : binds)
        return $ t1 ++ t2
    getRank2Targets (Case e _ _ alts) binds = do
        t1 <- getRank2Targets e binds
        t2 <- getRank2Targets alts binds
        return $ t1 ++ t2
    getRank2Targets (Cast e _) binds = getRank2Targets e binds
    getRank2Targets (Tick _ e) binds = getRank2Targets e binds
    -- For Types, Coercions, Literals etc, there can be no targets.
    getRank2Targets _ _ = return []

    -- There are two main cases for getTargets. The first is the case of a valid
    -- LHS (i.e. we are traversing something like lhs = ...) and ... is a lambda
    -- expression \x.\y.e. In this case, just add x to the lambda bound vars
    -- and continue descending into \y.e.
    getTargets (Lam b e@(Lam _ _)) (Just x) lbvs targets binds =
        getTargets e (Just x) (lbvs ++ [b]) targets binds
    -- The next case is again a valid LHS (i.e. lhs = ...) and we are currently
    -- descending into a lambda expression \y. e where e is not a lambda
    -- expression (otherwise, this would be handled by the first case). This is
    -- where the main interesting thing happens.
    getTargets (Lam b e) (Just x) lbvs targets binds = do
        -- all_vars is all the lambda bound variables up to the expression e. We
        -- need this to compute where x is a target.
        let all_vars = lbvs ++ [b]
        -- Get all the function applications in e (as FnAppViews).
        let appViews = getAppViews e targets
        -- Based on the application views, we should see whether any of them are
        -- invocations of a target. Based on those invocations, determine whether
        -- a substitution of any of the variables in all_vars will cause the target
        -- applications to become partially static. Whichever variables make the
        -- targets partially static will also become targets for optimization.
        let new_indices = matchAppViewsToTargets all_vars binds appViews targets
        -- Once we have the indices, we just want to make sure that none of them
        -- are empty, then let the current function (the LHS x) be a target in
        -- those non-empty indices. The reason why the indices should not be empty
        -- is because if the original target application is already partially static,
        -- the expression can be optimized directly.
        let new_targets = map (x,) $ filter (not . null) new_indices
        -- Add those new targets to the list of targets and continue descending
        -- into the expression.
        getTargets e Nothing [] (new_targets ++ targets) binds
    -- This case is slightly different to the above, since there is no LHS we
    -- are relying on.
    getTargets (Lam _ e) _ _ targets binds = getTargets e Nothing [] targets binds
    -- For the remaining cases, just descend accordingly.
    getTargets (Let b e) _ _ targets binds = do
        t1 <- getTargets b Nothing [] targets (b : binds)
        getTargets e Nothing [] t1 (b : binds)
    getTargets (App f x) _ _ targets binds = do
        t1 <- getTargets f Nothing [] targets binds
        getTargets x Nothing [] t1 binds
    getTargets (Case e _ _ alts) _ _ targets binds = do
        t1 <- getTargets e Nothing [] targets binds
        getTargets alts Nothing [] t1 binds
    getTargets (Cast e _) _ _ targets binds = getTargets e Nothing [] targets binds
    getTargets (Tick _ e) _ _ targets binds = getTargets e Nothing [] targets binds
    getTargets _ _ _ targets _ = return targets

instance (Targettable a) => Targettable [a] where
    -- These are all quite straightforward since we just descend accordingly and
    -- combine the results
    getRank2Targets [] _ = return []
    getRank2Targets (x : xs) binds = do
        t1 <- getRank2Targets x binds
        t2 <- getRank2Targets xs binds
        return $ t1 ++ t2
    getTargets [] _ _ t _ = return t
    getTargets (x : xs) lhs vs t binds = do
        t1 <- getTargets x lhs vs t binds
        getTargets xs lhs vs t1 binds

instance Targettable CoreBind where
    -- When finding rank-2 targets, it is important to also see if the LHS binders
    -- have types that can also be targets.
    getRank2Targets (NonRec b e) binds = do
        t1 <- getRank2Targets (Var b :: CoreExpr) binds
        t2 <- getRank2Targets e binds
        return $ t1 ++ t2
    -- The checking of rank-2 targets of binders is done in the next type class
    -- instance.
    getRank2Targets (Rec ls) binds = getRank2Targets ls binds

    -- When descending into a NonRec bind, start checking if b = e can
    -- become an indirect target.
    getTargets (NonRec b e) _ _ = getTargets e (Just b) []
    -- The checking of b = e being indirect targets is done in the next type class
    -- instance.
    getTargets (Rec ls) _ _ = getTargets ls Nothing []

instance Targettable (Id, CoreExpr) where
    -- Check if the binder b is also a rank-2 type.
    getRank2Targets (b, e) binds = do
        t1 <- getRank2Targets (Var b :: CoreExpr) binds
        t2 <- getRank2Targets e binds
        return $ t1 ++ t2

    -- Start checking if b = e can become an indirect target.
    getTargets (b, e) _ _ = getTargets e (Just b) []

instance Targettable (Alt Var) where
    -- These are straightforward as they just descend into Alts.
    getRank2Targets (Alt _ _ e) = getRank2Targets e

    getTargets (Alt _ _ e) _ _ = getTargets e Nothing []

-- Continues to call getTargets until a fixed point is reached.
recursivelyAcquireTargets :: Targets -> [CoreBind] -> CoreM Targets
recursivelyAcquireTargets targets binds = do
    t' <- getTargets binds Nothing [] targets binds
    let t = nub t'
    if t == targets
        then return t
        else recursivelyAcquireTargets t binds

-------------------------------------------------------------------------------
--
-- Determining if something is rank-2 polymorphic
--
-- The way we do so is quite straightforward---just inspect the type. Which
-- ever function argument is polymorphic, we treat the entire type as being
-- rank-2 polymorphic in the index of that argument. We only really care about
-- the first argument; if it is rank-2 polymorphic in multiple arguments,
-- we can deal with that later once things have been optimized.
--
-- However, class methods must be targets in its dictionary argument also.
-- That is the purpose of dictArgIndex.
--
-------------------------------------------------------------------------------

-- Given a type, determine the indices of all the arguments that are
-- themselves polymorphic.
typeRank2 :: Type -> Indices
typeRank2 t
    | Just (_, t') <- splitForAllTyVar_maybe t = (+ 1) <$> typeRank2 t'
    | Just (_, _, a, r) <- splitFunTy_maybe t =
        case splitForAllTyVar_maybe a of
            Just _ -> [0]
            Nothing ->
                let something = typeRank2 a
                 in if not (null something)
                        then [0]
                        else (+ 1) <$> typeRank2 r
    | otherwise = []

-- Finds the index of a dictionary argument. We presume it is always the first
-- non-type argument of a ClassOp.
dictArgIndex :: Type -> Int
dictArgIndex t
    | Just (_, t') <- splitForAllTyVar_maybe t = dictArgIndex t' + 1
    | Just _ <- splitFunTy_maybe t = 0
    | otherwise = panic "Impossible! Function is supposed to receive dictionary argument but does not!"

-------------------------------------------------------------------------------
--
-- An FnAppView (function application view) allows us to inspect the arguments
-- to a function application in a nice flattened way, ignoring casts (which are
-- no-ops anyway). The Id is the function name, the [Coercion] stores any casts
-- that are attached to the Id (e.g. f |> c), and the list contains arguments
-- and casts on that intermediate application. The invariant is that the
-- rightmost argument will have no cast (the cast can simply be not considered
-- as part of the application). Also, the argument list must obviously be
-- nonempty.
--
-- As an example:
--  ((((f w) |> c) x y) |> d) z should be represented as an FnAppView:
--    FnAppView f Nothing [(w, Just c), (x Nothing), (y, Just d), (z, Nothing)]
--
-------------------------------------------------------------------------------
data FnAppView = FnAppView Id [Coercion] [(CoreExpr, [Coercion])]

-- This function turns an expression into a FnAppView. If the expression is not
-- a function application, or if it does not follow the typical structure we
-- are looking for (f x ... z ignoring casts and where f is an Id), then this
-- function returns Nothing.
exprToFnAppView :: CoreExpr -> Maybe FnAppView
exprToFnAppView expr = aux expr [] []
  where
    -- auxiliary function where everything happens.
    aux ::
        CoreExpr ->
        -- The current intermediate list of arguments and coercions
        [(CoreExpr, [Coercion])] ->
        -- Stores any coercions that are wrapping the current application
        [Coercion] ->
        Maybe FnAppView
    -- A var that has no arguments to it so far is not considered an application.
    aux (Var _) [] _ = Nothing
    -- A var with a nonempty argument list means that we have arrived at the
    -- "root" of the function application, and we can return the FnAppView.
    aux (Var v) ls c = Just $ FnAppView v c ls
    -- An App means we can discharge the cast and store it alongside the current
    -- argument, and continue computing the remainder of the application
    aux (App f x) ls c = aux f ((x, c) : ls) []
    -- A cast on an empty argument list means we have not entered a function
    -- application yet, and thus can directly return Nothing.
    aux (Cast _ _) [] _ = Nothing
    -- A cast with a nonempty argument list means that we should probably
    -- continue down an application, ignoring this cast. There are two cases.
    -- The first is where there is no cast on this application so far. In which
    -- case, we can just simply add the coercion into the current cast.
    aux (Cast e c) ls cs = aux e ls (c : cs)
    -- In all other cases, like let bindings or lambda expressions, just
    -- return Nothing as there is nothing to do.
    aux _ _ _ = Nothing

-- This class get every application of a function that is a target. The
-- arguments do not necessarily have to be partially static in the target
-- arguments. This is similar to exprToFnAppView except that every largest
-- FnAppView is obtained (largest in the sense that given f x y z then
-- only the view of f x y z is obtained, not f x, f x y and f x y z).
class GetAppViews a where
    getAppViews :: a -> Targets -> [FnAppView]

instance GetAppViews CoreExpr where
    getAppViews expr targets = aux expr [] []
      where
        -- aux is where the real stuff happens.
        aux ::
            CoreExpr ->
            -- The argument list so far.
            [(CoreExpr, [Coercion])] ->
            -- The current list of casts that have been descended through to get
            -- to this point
            [Coercion] ->
            [FnAppView]
        -- If there have been no arguments, Vars are not applications.
        aux (Var _) [] _ = []
        -- There are arguments, so simply check if the variable is a target
        -- and we can go on our way.
        aux (Var v) ls c
            | v `elem` map fst targets = [FnAppView v c ls]
            | otherwise = []
        -- When descending into applications, discharge whatever casts we have
        -- leading up to this point and add the argument to the argument list.
        -- Also, we descend into the argument in case there are important
        -- function applications in it.
        aux (App f x) ls c =
            let res1 = aux f ((x, c) : ls) []
                res2 = aux x [] []
             in res1 ++ res2
        -- A cast with no argument so far is lame, we just descend into the
        -- expression being cast.
        aux (Cast e _) [] _ = aux e [] []
        -- If a cast has arguments, it means that we are midway through
        -- descending into a function application. All we do is add the
        -- coercion into the coercion list and continue.
        aux (Cast e c) ls ls' = aux e ls (c : ls')
        -- The remaining cases are simple.
        aux (Lam _ e) _ _ = aux e [] []
        aux (Let b e) _ _ =
            let res1 = getAppViews b targets
                res2 = aux e [] []
             in res1 ++ res2
        aux (Case e _ _ alts) _ _ =
            let res1 = aux e [] []
                res2 = getAppViews alts targets
             in res1 ++ res2
        aux (Tick _ e) _ _ = aux e [] []
        aux _ _ _ = []

-- The other cases are very simple, and we just descend further into the structures.
instance (GetAppViews a) => GetAppViews [a] where
    getAppViews ls targets =
        let res = map (`getAppViews` targets) ls
         in concat res

instance GetAppViews CoreBind where
    getAppViews b = getAppViews (rhssOfBind b)

instance GetAppViews (Alt Var) where
    getAppViews (Alt _ _ e) = getAppViews e

-------------------------------------------------------------------------------
--
-- Indirect partially static applications
--
-- The matchAppViewsToTargets essentially determines, given
--    lhs = \x₁.\x₂. ...\xₙ. e
-- and a bunch of function applications (f ...) in e where f are target
-- functions, whether a nonempty substitution of some set of xᵢ for partially
-- static terms will result in some of those function applications (f ...) in
-- also becoming partially static.
--
-- The bindingTimeAnalysis function is really important here as it is what
-- decides whether a term is partially static or not.
--
-------------------------------------------------------------------------------

-- Returns a set of indices such that substituting the lambda bound vars (of
-- those indices) with partially static terms will cause some function
-- applications ([FnAppView]) to become partially static (in the targets).
--
-- The invariant here is that the [FnAppView] must all be function applications
-- of **some** target.
matchAppViewsToTargets ::
    LambdaBoundVars ->
    InScopeBinds ->
    [FnAppView] ->
    Targets ->
    [Indices]
matchAppViewsToTargets _ _ [] _ = return []
matchAppViewsToTargets lbvs in_scope_binds (FnAppView f _ args : xs) targets =
    let
        -- The very first step is to perform a binding-time analysis of the
        -- arguments of the function application
        bta_args = map ((`bindingTimeAnalysis` in_scope_binds) . fst) args

        -- Keep only those targets that are relevant, i.e., that are relevant to
        -- the current function application being matched, and the function
        -- application has enough args to ever become optimized.
        relevant_targets =
            filter (\(f', is) -> (f == f') && largestIndex is < length args) targets

        -- Here, just map the targets into the analyses results of the arguments.
        -- Now, instead of each target storing indices, they store the binding
        -- time analysis results of the corresponding function arguments.
        targets_to_bta :: (Id, Indices) -> [BindingTimeAnalysisResult]
        targets_to_bta (_, indices) = map (bta_args !!) indices
        args_of_targets = map targets_to_bta relevant_targets

        -- We want to remove any targets such that there are arguments that can
        -- never become partially static, based on the binding time analysis.
        filtered_args = filter args_can_never_be_ps args_of_targets

        -- The binding-time analysis also reveals what variables must be
        -- partially static for the argument to become partially static. We use
        -- this information to get all the required variables.
        required_vars = map collect_vars filtered_args

        -- The final result is to match those variables with the lambda-bound
        -- variables we have at our disposal.
        res = map matchVarsWithLBVs required_vars
     in
        res ++ matchAppViewsToTargets lbvs in_scope_binds xs targets
  where
    -- Determines if any of the function arguments can never be partially
    -- static.
    args_can_never_be_ps :: [BindingTimeAnalysisResult] -> Bool
    args_can_never_be_ps = notElem CanNeverBePartiallyStatic

    -- Collects all the required variables in the argument list.
    collect_vars :: [BindingTimeAnalysisResult] -> [Var]
    collect_vars [] = []
    collect_vars (NeedsVariables ls : xs') = ls ++ collect_vars xs'
    collect_vars (_ : xs') = collect_vars xs'

    -- Matches the required variables with the lambda-bound variables that
    -- we have at our disposal.
    matchVarsWithLBVs :: [Var] -> Indices
    matchVarsWithLBVs ls
        -- If any of the required variables are not lambda-bound variables,
        -- then it is hopeless and we can just return [].
        | any (`notElem` lbvs) ls = []
        -- Otherwise we perform the fold, below.
        | otherwise = foldLBVsToIndices lbvs ls
    -- The fold just converts required variables to indices (based on the
    -- lambda-bound variables list we have).
    foldLBVsToIndices [] _ = []
    foldLBVsToIndices (x : xs') ls =
        let xs'' = (+ 1) <$> foldLBVsToIndices xs' ls
         in if x `elem` ls then xs'' @+ 0 else xs''

-------------------------------------------------------------------------------
--
-- Binding-Time Analysis
--
-- In this optimization pass we need a notion of "partially static" and a way
-- of computing it. The bindingTimeAnalysis does exactly that; given an
-- expression and some in-scope binds (which is needed for determining if a
-- variable has a bind whose RHS is partially static), it reports one of three
-- possible results:
--    1. The expression is already partially static
--    2. The expression can never be partially static (for instance, the x in
--       let x = x in x)
--    3. The expression is not partially static, but can be made partially
--       static by substituting a list of variables with partially static
--       terms.
--
-------------------------------------------------------------------------------

-- The result of a binding-time analysis.
data BindingTimeAnalysisResult
    = AlreadyPartiallyStatic
    | CanNeverBePartiallyStatic
    | NeedsVariables [Var]
    deriving (Eq)

-- Determines if an expression is partially static. This function just makes
-- use of bindingTimeAnalysis.
isPSTerm :: InScopeBinds -> CoreExpr -> Bool
isPSTerm in_scope_binds e =
    AlreadyPartiallyStatic == bindingTimeAnalysis e in_scope_binds

bindingTimeAnalysis :: CoreExpr -> InScopeBinds -> BindingTimeAnalysisResult
bindingTimeAnalysis xp iss = snd (aux xp iss [])
  where
    aux ::
        -- The expression to analyze
        CoreExpr ->
        -- The in-scope bindings
        InScopeBinds ->
        -- The memo
        [CoreExpr] ->
        -- The new memo and the analysis result
        ([CoreExpr], BindingTimeAnalysisResult)
    -- If an expression can be found in the memo, then forget about it.
    aux e _ memo
        | any (\x -> deBruijnize e == deBruijnize x) memo =
            (e : memo, CanNeverBePartiallyStatic)
    aux (Var v) in_scope_binds memo
        -- If v has a binding, the result depends on the RHS of the binding
        | v `boundIn` in_scope_binds =
            let rhs = lookupTopLevelRHS v in_scope_binds
             in aux rhs in_scope_binds (Var v : memo)
        -- Otherwise, it is a localId, and thus is (likely) some lambda-bound
        -- variable, and thus needs to be substituted to become partially static
        | isLocalId v = (Var v : memo, NeedsVariables [v])
        -- Otherwise, it is some other kind of ID. More than likely this ID is
        -- already partially static.
        | otherwise = (Var v : memo, AlreadyPartiallyStatic)
    aux e@(App f x) in_scope_binds memo₁ =
        -- traverse into f first
        case aux f in_scope_binds (e : memo₁) of
            -- If f is partially static, then f x is also partially static.
            (memo₂, AlreadyPartiallyStatic) -> (memo₂, AlreadyPartiallyStatic)
            -- If f can never be partially static, then f x depends solely on
            -- the analysis of x
            (memo₂, CanNeverBePartiallyStatic) -> aux x in_scope_binds memo₂
            -- Otherwise, if f needs variables, we analyze x:
            (memo₂, NeedsVariables ls) -> case aux x in_scope_binds memo₂ of
                -- if x is partially static, then f x is partially static
                (memo₃, AlreadyPartiallyStatic) -> (memo₃, AlreadyPartiallyStatic)
                -- if x can never be partially static, we use the analysis results
                -- from f
                (memo₃, CanNeverBePartiallyStatic) -> (memo₃, NeedsVariables ls)
                -- otherwise, concatenate all the variables together. Technically
                -- we can do either ls or ls', but we can make the conditions
                -- stricter and require more variables than necessary.
                (memo₃, NeedsVariables ls') -> (memo₃, NeedsVariables (ls ++ ls'))
    -- A cast is no-op so it doesn't affect the analysis results of the
    -- sub-expression.
    aux expr@(Cast e _) in_scope_binds memo = aux e in_scope_binds (expr : memo)
    -- A tick is no-op so it doesn't affect the analysis results of the
    -- sub-expression.
    aux expr@(Tick _ e) in_scope_binds memo = aux e in_scope_binds (expr : memo)
    -- Type arguments are interesting in that types are not expressions;
    -- we use the convention that type variables are not partially static,
    -- type applications are similar to function applications, and that
    -- all other types are already partially static. Realistically, the results
    -- here are never used to decide if a target can be optimized or not.
    aux expr@(Type t) in_scope_binds memo₁
        | Just v <- getTyVar_maybe t = (expr : memo₁, NeedsVariables [v])
        | Just (t1, t2) <- splitAppTy_maybe t =
            case aux (Type t1) in_scope_binds (expr : memo₁) of
                (memo₂, AlreadyPartiallyStatic) -> (memo₂, AlreadyPartiallyStatic)
                (memo₂, CanNeverBePartiallyStatic) ->
                    aux (Type t2) in_scope_binds memo₂
                (memo₂, NeedsVariables ls) ->
                    case aux (Type t2) in_scope_binds memo₂ of
                        (memo₃, AlreadyPartiallyStatic) -> (memo₃, AlreadyPartiallyStatic)
                        (memo₃, CanNeverBePartiallyStatic) -> (memo₃, NeedsVariables ls)
                        (memo₃, NeedsVariables ls') -> (memo₃, NeedsVariables (ls ++ ls'))
        | otherwise = (expr : memo₁, AlreadyPartiallyStatic)
    -- Only coercion variables are not partially static.
    aux expr@(Coercion c) _ memo
        | Just v <- getCoVar_maybe c = (expr : memo, NeedsVariables [v])
        | otherwise = (expr : memo, AlreadyPartiallyStatic)
    -- Simply propagate the analysis downward.
    aux expr@(Let b e) in_scope_binds memo = aux e (b : in_scope_binds) (expr : memo)
    -- Everything else is partially static.
    aux e _ memo = (e : memo, AlreadyPartiallyStatic)

-------------------------------------------------------------------------------
--
-- The main optimization
--
-------------------------------------------------------------------------------

type NewMemoTable = MemoTable

type IsTransformed = Bool

type FloatedBind = (Id, CoreExpr)

class Optimizable a where
    -- | Performs the actual transformation on the term.
    optimize ::
        -- | The thing to transform
        a ->
        -- | The targets to look for
        Targets ->
        -- | The in-scope binds at that program point (used to check if a memoized
        -- expression is in scope)
        InScopeBinds ->
        -- | The memoization table
        MemoTable ->
        -- | The transformed expression, the new memoization table, whether it was
        -- transformed, and any floated binding. The 'IsTransformed' flag is required
        -- as they may be no floated binding, but the expression may still be
        -- transformed.
        -- Invariant 1: If 'IsTransformed' is 'False', then the 'FloatedBind' is
        -- 'Nothing'.
        -- Invariant 2: As long as the target of transformation has been changed
        -- in any way, the 'IsTransformed' flag is 'True'.
        CoreM (a, NewMemoTable, IsTransformed, Maybe FloatedBind)

instance Optimizable CoreExpr where
    -- The case where a transformation can be performed.
    optimize expr targets in_scope_binds memo
        -- expr is a partially static application of a target. Optimize!
        | Just app_view <-
            getPartiallyStaticAppOfTarget expr targets in_scope_binds = do
            -- Check to see if the expression is already memoized
            let possible_memoized = getMemoized expr memo
                -- Make sure that the memoized expressions are in scope
                in_scope_memoized =
                    filter (`boundIn` in_scope_binds) possible_memoized
            case in_scope_memoized of
                [] -> do
                    -- If not memoized, create a new binding
                    -- Get the type of the expression
                    let expr_type = exprType expr
                    -- Create a new identifier for the binding
                    new_id <- mkNewIdentifier expr_type
                    -- Transform the expression (we use the application view for this)
                    (sated_e, memo_e) <- transformRHS app_view in_scope_binds
                    -- The floated bind is the new identifier and the transformed
                    -- expression
                    let floated_bind = (new_id, sated_e)
                        -- Add a new entry to the memoization table
                        new_memo = map (,new_id) memo_e ++ memo
                        -- The new expression is just the new identifier generated.
                        new_expr = Var new_id
                    return (new_expr, new_memo, True, Just floated_bind)
                -- If it is already memoized, just return the variable
                (x : _) -> return (Var x, memo, True, Nothing)
    optimize (App f x) targets in_scope_binds memo = do
        -- Pretty simple since an application introduces no bindings. Whatever
        -- let-bind is generated can be simply floated out.
        (f', memo₁, flag₁, floated_binds₁) <- optimize f targets in_scope_binds memo
        if flag₁
            then do
                -- If the function was transformed, we should not transform the argument.
                -- Just return the application with the transformed function.
                return (App f' x, memo₁, True, floated_binds₁)
            else do
                -- Since there is nothing to do for the function, we can just transform
                -- the argument.
                (x', memo₂, flag₂, floated_binds₂) <-
                    optimize x targets in_scope_binds memo₁
                return (App f' x', memo₂, flag₂, floated_binds₂)
    optimize (Lam b e) targets in_scope_binds memo = do
        -- Transform the function body.
        (e', memo', flag, floated_binds) <- optimize e targets in_scope_binds memo
        -- Check if there is nothing to do
        if not flag
            then return (Lam b e', memo', False, floated_binds)
            else case floated_binds of
                -- Something happened.
                -- If there is no floated bind, just return the lambda with the
                -- transformed body.
                Nothing -> return (Lam b e', memo', True, Nothing)
                -- If there is a floated bind, check if it can also be floated
                -- again. The way to do so is to check if the expression's free
                -- variables includes the variable bound by the lambda.
                Just (floated_id, floated_expr) ->
                    if b `elementOfUniqSet` exprFreeVars floated_expr
                        then -- Cannot float again because the variable bound by the lambda
                        -- is used in the RHS of the let bind, so just return the lambda
                        -- with the let binding.

                            return
                                ( Lam b (Let (NonRec floated_id floated_expr) e')
                                , memo'
                                , True
                                , Nothing
                                )
                        else do
                            -- Can continue to float the let binding, so return the lambda
                            -- that does not contain the let binding
                            return (Lam b e', memo', True, floated_binds)
    optimize (Let b e) targets in_scope_binds memo = do
        -- Transform the bindings. Ensure that all the bindings are made in scope.
        (b', memo₁, flag₁, floated_binds₁) <-
            optimize b targets (b : in_scope_binds) memo
        if flag₁
            then -- Something happened.
            -- The handling of the floated binds should be done by the transformation
            -- of the binds. Just directly return.
                return (Let b' e, memo₁, True, floated_binds₁)
            else do
                -- If there is nothing to do, just transform the expression. Same
                -- thing: make sure the let bindings are in scope.
                (e', memo₂, flag₂, floated_binds₂) <-
                    optimize e targets (b : in_scope_binds) memo₁
                -- Now check if there is a floated bind from transforming the expression.
                case floated_binds₂ of
                    Nothing -> return (Let b' e', memo₂, flag₂, Nothing)
                    Just (floated_id, floated_expr) -> do
                        -- If there is a floated bind, we should check if it can be floated
                        -- further.
                        -- Get all the let-bound variables in the let binding.
                        let introduced_vars = bindersOf b'
                            -- Get all the free variables in the expression
                            free_vars = exprFreeVars floated_expr
                        -- Check if any of the let-bound vars are free vars in the expression.
                        if any (`elementOfUniqSet` free_vars) introduced_vars
                            then -- If so, we cannot float it further, so we just include the
                            -- floated bind in the let binding. Note that the
                            -- resulting binding is a recursive binding.
                            case b' of
                                NonRec x y ->
                                    return
                                        ( Let (Rec [(floated_id, floated_expr), (x, y)]) e'
                                        , memo₂
                                        , True
                                        , Nothing
                                        )
                                Rec ls ->
                                    return
                                        ( Let (Rec ((floated_id, floated_expr) : ls)) e'
                                        , memo₂
                                        , True
                                        , Nothing
                                        )
                            else -- If not, we can float it further.
                                return (Let b' e', memo₂, True, floated_binds₂)
    optimize (Case e b t alts) targets in_scope_binds memo = do
        -- Perform the transformation on the expression.
        (e', memo₁, flag₁, floated_binds₁) <- optimize e targets in_scope_binds memo
        if flag₁
            then -- It was transformed; just float out the floated binds.
                return (Case e' b t alts, memo₁, True, floated_binds₁)
            else do
                -- If there is nothing to do, just transform the alts.
                (alts', memo₂, flag₂, floated_binds₂) <-
                    optimize alts targets in_scope_binds memo₁
                -- Check if b occurs in the floated binds.
                case floated_binds₂ of
                    Nothing -> return (Case e' b t alts', memo₂, flag₂, floated_binds₂)
                    Just (floated_id, floated_expr) ->
                        let free_vars = exprFreeVars floated_expr
                         in if b `elementOfUniqSet` free_vars
                                then -- Cannot float out, so it is a little bit tricky. Best way
                                -- to proceed is to put a let binding with the expression
                                -- and the bind.

                                    return
                                        ( Let
                                            (Rec [(floated_id, floated_expr), (b, e')])
                                            (Case e' b t alts')
                                        , memo₂
                                        , flag₂
                                        , Nothing
                                        )
                                else return (Case e' b t alts', memo₂, flag₂, floated_binds₂)
    optimize (Cast e c) targets in_scope_binds memo = do
        -- Very straightforward: just transform the expression.
        (e', memo', flag, floated_binds) <- optimize e targets in_scope_binds memo
        return (Cast e' c, memo', flag, floated_binds)
    optimize (Tick t e) targets in_scope_binds memo = do
        -- Very straightforward: just transform the expression.
        (e', memo', flag, floated_binds) <- optimize e targets in_scope_binds memo
        return (Tick t e', memo', flag, floated_binds)
    -- For all other cases, there is nothing to do!
    optimize e _ _ memo = return (e, memo, False, Nothing)

instance Optimizable (Alt Var) where
    optimize (Alt alt_con b e) targets in_scope_binds memo = do
        -- Pretty straightforward; just descend into e and try to float out the
        -- binding.
        (e', memo', flag, floated_binds) <- optimize e targets in_scope_binds memo
        case floated_binds of
            Nothing -> return (Alt alt_con b e', memo', flag, Nothing)
            Just (floated_id, floated_expr) -> do
                -- If there is a floated bind, we should check if it can be floated
                -- further.
                -- Get all the free variables in the floated expression.
                let free_vars = exprFreeVars floated_expr
                -- Check if any of the alt-bound variables are free variables in the expression.
                if any (`elementOfUniqSet` free_vars) b
                    then -- If so, we cannot float it further, so we put the floated binds
                    -- in a let binding.

                        return
                            ( Alt alt_con b (Let (NonRec floated_id floated_expr) e')
                            , memo'
                            , True
                            , Nothing
                            )
                    else -- can float it further!
                        return (Alt alt_con b e', memo', True, floated_binds)

instance Optimizable CoreBind where
    optimize (NonRec b e) targets in_scope_binds memo = do
        (e', memo', flag, floated_binds) <- optimize e targets in_scope_binds memo
        case floated_binds of
            -- No binds to float out; just return the let binding with the
            -- transformed body.
            Nothing -> return (NonRec b e', memo', flag, Nothing)
            -- There is a floated bind, so we should check if it can be
            -- floated further.
            Just (floated_id, floated_expr) -> do
                -- Get all the free variables in the expression
                let free_vars = exprFreeVars floated_expr
                -- Check if any of the let-bound vars are free vars in the
                -- expression.
                if b `elementOfUniqSet` free_vars
                    then -- If so, we cannot float it further, so we just include the
                    -- floated bind in the let binding. Note that the
                    -- resulting binding is a recursive binding.

                        return
                            ( Rec [(floated_id, floated_expr), (b, e)]
                            , memo'
                            , True
                            , Nothing
                            )
                    else -- If not, we can float it further.
                        return (NonRec b e', memo', True, floated_binds)
    optimize (Rec ls) targets in_scope_binds memo = do
        (ls', memo', flag, floated_binds) <- optimize ls targets in_scope_binds memo
        case floated_binds of
            -- No binds to float out; just return the let binding with the
            -- transformed body.
            Nothing -> return (Rec ls', memo', flag, Nothing)
            -- There is a floated bind, so we should check if it can be
            -- floated further.
            Just (floated_id, floated_expr) -> do
                -- If there is a floated bind, we should check if it can be floated
                -- further.
                -- Get all the let-bound variables in the let binding.
                let introduced_vars = bindersOf (Rec ls')
                    -- Get all the free variables in the expression
                    free_vars = exprFreeVars floated_expr
                -- Check if any of the let-bound vars are free vars in the expression.
                if any (`elementOfUniqSet` free_vars) introduced_vars
                    then -- If so, we cannot float it further, so we just include the
                    -- floated bind in the let binding. Note that the
                    -- resulting binding is a recursive binding.

                        return
                            ( Rec ((floated_id, floated_expr) : ls')
                            , memo'
                            , flag
                            , Nothing
                            )
                    else -- If not, we can float it further.

                        return (Rec ls', memo', True, floated_binds)

-- The rest of the cases are mere descents into the subterms.
instance (Optimizable a) => Optimizable [a] where
    optimize [] _ _ memo = return ([], memo, False, Nothing)
    optimize (x : xs) targets in_scope_binds memo = do
        (x', memo1, flag, floated_binds) <- optimize x targets in_scope_binds memo
        if not flag
            then do
                (xs', memo2, flag2, floated_binds2) <-
                    optimize xs targets in_scope_binds memo1
                return (x' : xs', memo2, flag2, floated_binds2)
            else return (x' : xs, memo1, flag, floated_binds)

instance Optimizable (Id, CoreExpr) where
    optimize (i, e) targets in_scope_binds memo = do
        (e', memo1, flag, floated_binds) <- optimize e targets in_scope_binds memo
        return ((i, e'), memo1, flag, floated_binds)

-- Inspect an expression and obtain a partially static target function
-- application, if any
getPartiallyStaticAppOfTarget ::
    CoreExpr ->
    Targets ->
    InScopeBinds ->
    Maybe FnAppView
getPartiallyStaticAppOfTarget e t binds = do
    -- Obtain the cast-insensitive function application decomposition
    app_view@(FnAppView f _ args) <- exprToFnAppView e
    -- Filter all targets that do not involve the function symbol in this expression
    let relevant_targets = filter (\(x, _) -> f == x) t
    -- The arguments to this function application has to be partially static
    guard $ any (all_target_args_partially_static (map fst args)) relevant_targets
    return app_view
  where
    all_target_args_partially_static :: [CoreExpr] -> (Id, Indices) -> Bool
    all_target_args_partially_static args (_, is) =
        largestIndex is == length args - 1
            && ( let args_that_must_be_ps = map (args !!) is
                  in all (isPSTerm binds) args_that_must_be_ps
               )

-- Actually do the transformation, where we inline the function symbol
-- and any dictionary arguments, and simplify the entire expression.
-- Also create some memoization records for this expression
transformRHS :: FnAppView -> InScopeBinds -> CoreM (CoreExpr, [CoreExpr])
transformRHS (FnAppView f c args) binds = do
    let (real_args, coers) = unzip args
    -- Unfold the function symbol
    let inlined =
            if f `boundIn` binds
                then lookupTopLevelRHS f binds
                else unfoldingTemplate $ realIdUnfolding f
    -- Obtain simple inlinings of dictionary arguments. This is needed
    -- when the dictionary arguments are actually defined somewhere
    -- else in the file, locally, and unfoldings for this dictionary
    -- are not yet present
    memo_args <- mapM (fullTransformM (inlineDict binds)) real_args
    -- Perform dictionary inlining
    new_args <- case idDetails f of
        ClassOpId{} -> do
            let t = idType f
                i = dictArgIndex t
                d_arg = memo_args !! i
            dictArg <- leftInlineLikeCrazy d_arg
            return $ take i memo_args ++ (dictArg : drop (i + 1) memo_args)
        _ -> return memo_args
    -- Rebuild the function application with the new inlinings
    let full_args = zip new_args coers
    let applied = rebuildApp inlined c full_args
    -- Simplify
    beta'd <- betaReduceCompletelyM applied
    -- Create the memoization records (include one where the simple inlining
    -- of the dictionaries are inserted into the dictionary argument
    -- positions of the original expression, just in case we encounter it
    -- again)
    let original_expr = rebuildApp (Var f) c args
    let inline_dict_expr = rebuildApp (Var f) c (zip memo_args coers)
    return (beta'd, [original_expr, inline_dict_expr])

type InlinedFunction = CoreExpr

-- Rebuild a function application given the cast-insensitive function
-- decomposition.
rebuildApp ::
    InlinedFunction ->
    [Coercion] ->
    [(CoreExpr, [Coercion])] ->
    CoreExpr
rebuildApp f c ls =
    let fn = foldl Cast f c
        -- This foldArgs function just takes every single argument in the
        -- argument list and rebuilds it with App, potentially wrapping them
        -- with casts as necessary.
        foldArgs :: CoreExpr -> [(CoreExpr, [Coercion])] -> CoreExpr
        -- Folding the expression over the empty list is trivial
        foldArgs e [] = e
        -- For other casts, we have to discriminate between the two cases where
        -- there is a cast to be performed or not.
        foldArgs e ((arg, cs) : xs) = foldArgs (foldl Cast (App e arg) cs) xs
     in -- The result is just folding fn over the argument list using our foldArgs
        -- function.
        foldArgs fn ls

-- Attempts to inline a dictionary from somewhere in this program
inlineDict :: CoreProgram -> CoreExpr -> CoreM CoreExpr
inlineDict pgm (Var v) = do
    let lhss = pgm >>= bindersOf
    if v `elem` lhss
        then do
            let rhs = lookupTopLevelRHS v pgm
                av = exprToFnAppView rhs
            case av of
                Just (FnAppView dfun _ _) -> case realIdUnfolding dfun of
                    DFunUnfolding{} -> return rhs
                    _ -> return $ Var v
                Nothing -> return $ Var v
        else return $ Var v
inlineDict _ e = return e

-- Looks up the RHS of a top-level binding given an ID
lookupTopLevelRHS :: Id -> [CoreBind] -> CoreExpr
lookupTopLevelRHS id' pgm =
    -- Just a basic lookup. Quite straightforward.
    case lookupTopLevelRHS_maybe pgm of
        Just x -> x
        Nothing -> panic "the impossible happened... cannot find RHS of bind"
  where
    lookupTopLevelRHS_maybe :: [CoreBind] -> Maybe CoreExpr
    lookupTopLevelRHS_maybe [] = Nothing
    lookupTopLevelRHS_maybe (NonRec lhs rhs : bs)
        | id' == lhs = Just rhs
        | otherwise = lookupTopLevelRHS_maybe bs
    lookupTopLevelRHS_maybe (Rec ls : bs) =
        case aux ls of
            Nothing -> lookupTopLevelRHS_maybe bs
            x -> x
    aux :: [(Id, CoreExpr)] -> Maybe CoreExpr
    aux [] = Nothing
    aux ((lhs, rhs) : rs)
        | id' == lhs = Just rhs
        | otherwise = aux rs

-- | Creates a new identifier for the LHS of a generated bind.
mkNewIdentifier ::
    -- | The type of the identifier
    Type ->
    -- | The identifier itself.
    CoreM Id
mkNewIdentifier t = do
    -- get the unique values from the monad
    uniq1 <- getUniqueM
    uniq2 <- getUniqueM
    -- Make the name. The name is a local occurrence and should not have to be
    -- exported (it will likely be a local variable). We just use
    -- 'UnhelpfulSpan' and 'UnhelpfulGenerated' since this ID is generated
    -- by us.
    let name =
            mkInternalName
                uniq1
                (mkLocalOcc uniq2 (mkVarOcc "class_spec"))
                (UnhelpfulSpan UnhelpfulGenerated)
    -- The multiplicity is 'ManyTy' because, well, who cares.
    return $ mkLocalId name ManyTy t

-- This essentially keeps inlining the dictionary until it has revealed
-- an actual DFun, exposing the methods in the dictionary for class-dictionary
-- specialization.
leftInlineLikeCrazy :: CoreExpr -> CoreM CoreExpr
leftInlineLikeCrazy = leftElaborationLikeCrazy extractor inlineId
  where
    extractor :: CoreExpr -> Maybe Id
    extractor (Var v) = Just v
    extractor _ = Nothing
    inlineId :: Id -> CoreM CoreExpr
    inlineId v = do
        let uf = realIdUnfolding v
        case uf of
            DFunUnfolding bndrs df_con df_args -> do
                let dfune = mkCoreConApps df_con df_args
                let dfun = foldr Lam dfune bndrs
                return dfun
            CoreUnfolding tmpl _ _ _ _ -> do
                return tmpl
            OtherCon _ -> do
                return $ Var v
            NoUnfolding -> do
                return $ Var v
            _ -> do
                return $ Var v

type MemoTable = [(CoreExpr, Var)]

-- Simple function to obtain the memoization record(s).
getMemoized :: CoreExpr -> MemoTable -> [Var]
getMemoized _ [] = []
getMemoized e ((e', v) : xs) =
    let xs' = getMemoized e xs
     in if deBruijnize e == deBruijnize e'
            then v : xs'
            else xs'
