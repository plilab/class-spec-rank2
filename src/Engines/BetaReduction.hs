-- A simple module for performing beta reduction.
{-# LANGUAGE LambdaCase #-}

module Engines.BetaReduction where

import Engines.Transform
import GHC.Core.Map.Type
import GHC.Data.Pair
import GHC.Plugins

-- Obtains the free variables of an expression.
getFVs :: CoreExpr -> InScopeSet
getFVs e = extendInScopeSetList emptyInScopeSet (exprFreeVarsList e)

-- Actually perform a substitution.
performSubstitution' :: Var -> CoreExpr -> CoreExpr -> CoreExpr
performSubstitution' var (Coercion c) expr
    | isCoVar var =
        let iss = getFVs expr
            subst = Subst iss emptyVarEnv emptyVarEnv (unitVarEnv var c)
            new_expr = substExpr subst expr
         in new_expr
    | otherwise = panic "Substitution on coercion argument but not on coercion variable!"
performSubstitution' var (Type t) expr
    | isTyVar var =
        let iss = getFVs expr
            subst = Subst iss emptyVarEnv (unitVarEnv var t) emptyVarEnv
            new_expr = substExpr subst expr
         in new_expr
    | otherwise = panic "Substitution on type argument but not on type variable!"
performSubstitution' var e expr
    | isId var =
        let iss = getFVs expr
            subst = Subst iss (unitVarEnv var e) emptyVarEnv emptyVarEnv
            new_expr = substExpr subst expr
         in new_expr
    | otherwise = panic "Substitution on term argument but not on term variable!"

-- Beta reduction.
betaReducerM :: CoreExpr -> CoreM CoreExpr
betaReducerM (Cast e c)
    | isReflexiveCo c = do
        return e
betaReducerM (Cast (Cast e c_1) c_2) = do
    let new_expr = Cast e (mkTransCo c_1 c_2)
    return new_expr
betaReducerM (Cast (Let b e) c) = do
    let new_expr = Let b (Cast e c)
    return new_expr
betaReducerM (Cast (Case e b _ alts) c) = do
    let Pair _ r = coercionKind c
    let new_alts = (\case (Alt ac b' e') -> Alt ac b' (Cast e' c)) <$> alts
    let new_expr = Case e b r new_alts
    return new_expr
betaReducerM (App (Lam var rhs) x) = do
    let new_expr = performSubstitution' var x rhs
    return new_expr
betaReducerM (Case e _ _ alts)
    | Just (datacon, _, term_arg) <- exprIsDataConAppMaybe e
    , Just (Alt (DataAlt _) vars rhs) <- getMatchingAltConMaybe datacon alts =
        do
            let new_expr = performSubstitutionList vars term_arg rhs
            return new_expr
betaReducerM (App (Cast e_1 c) (Type t)) = do
    let refl_c = mkReflCo Nominal t
    let new_expr = Cast (App e_1 (Type t)) (mkInstCo c refl_c)
    return new_expr
betaReducerM expr@(App (Let b e₁) e₂) = do
    let free_vars = exprFreeVars e₂
        bound_vars = bindersOf b
    if any (`elementOfUniqSet` free_vars) bound_vars
        then return expr
        else do
            let new_expr = Let b (App e₁ e₂)
            return new_expr
betaReducerM (App (Cast e_1 c) e_2)
    | let (Pair s1t1 s2t2) = coercionKind c
       in isFunTy s1t1 && isFunTy s2t2 = do
        let (_, nth_1, nth_2) = decomposeFunCo c
        let sym_nth_1 = mkSymCo nth_1
        let new_expr = Cast (App e_1 (Cast e_2 sym_nth_1)) nth_2
        return new_expr
betaReducerM x = return x

-- Determines if an expression is a DataCon. Use for matching case-of-known constructors.
exprIsDataConAppMaybe :: CoreExpr -> Maybe (DataCon, [Type], [CoreExpr])
exprIsDataConAppMaybe (App (Var id') (Type t)) = (,[t],[]) <$> isDataConId_maybe id'
exprIsDataConAppMaybe (App (Var id') e) = (,[],[e]) <$> isDataConId_maybe id'
exprIsDataConAppMaybe (App f (Type e))
    | Just (dc, ty_args, t_args) <- exprIsDataConAppMaybe f =
        Just (dc, ty_args ++ [e], t_args)
exprIsDataConAppMaybe (App f e)
    | Just (dc, ty_args, t_args) <- exprIsDataConAppMaybe f =
        Just (dc, ty_args, t_args ++ [e])
exprIsDataConAppMaybe _ = Nothing

-- Get a matching alternative in a case expression given a DataCon. Used
-- for matching case-of-known constructors.
getMatchingAltConMaybe :: DataCon -> [Alt Var] -> Maybe (Alt Var)
getMatchingAltConMaybe _ [] = Nothing
getMatchingAltConMaybe _ (Alt DEFAULT _ _ : _) = Nothing
getMatchingAltConMaybe dc (a@(Alt (DataAlt da) _ _) : _)
    | dc == da = Just a
getMatchingAltConMaybe dc (_ : xs) = getMatchingAltConMaybe dc xs

-- Performs a substitution over a list of Id -> Expr pairs.
performSubstitutionList :: [Id] -> [CoreExpr] -> CoreExpr -> CoreExpr
performSubstitutionList [] [] rhs = rhs
performSubstitutionList (v : vs) (b : bs) rhs =
    let new_expr = performSubstitution' v b rhs
     in performSubstitutionList vs bs new_expr
performSubstitutionList _ _ _ = panic "Mismatch of let-bound variables and expressions"

-- Beta reduction everywhere.
betaReduceM :: (FullTransform CoreExpr a) => a -> CoreM a
betaReduceM = fullTransformM betaReducerM

-- Beta reduction to a fixed point
betaReduceCompletelyM :: (Eq (DeBruijn a), FullTransform CoreExpr a) => a -> CoreM a
betaReduceCompletelyM = fullTransformMTillFixedPoint deBruijnize betaReducerM
