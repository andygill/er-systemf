module Language.SystemF.Subst where

import Language.SystemF.Syntax
import Data.List


substExp :: Exp -> (Name,Exp) -> Exp
substExp (Var n) (nm,r) | n == nm = r
                        | otherwise = Var n
substExp (Lit n)  _ = Lit n
substExp (App e1 e2) b = App (substExp e1 b) (substExp e2 b)
substExp (APP e t)   b = APP (substExp e b) t
substExp (Lam n t e) bd@(nm,r)
   | n == nm
   = Lam n t e
   | n `elem` freeExp r
   = Lam n' t (substExp (substExp e (n,Var n')) bd)
   | otherwise = Lam n t (substExp e bd)
  where
    n' = newName (freeExp e ++ freeExp r ++ [n]) n
substExp (LAM n e) bd@(_,r)
    | n `elem` tyFreeExp r
    = LAM n' (substExp (tySubstExp e (n,TyVar n')) bd)
    | otherwise
    = LAM n (substExp e bd)
  where
    n' = newName (tyFreeExp e ++ tyFreeExp r ++ [n]) n
substExp (Let (NonRecBinding (v,(ty,rhs))) e) bd@(nm,r) 
    | v == nm 
    = Let (NonRecBinding (v,(ty,substExp rhs bd))) e
    | v `elem` freeExp r
    = Let (NonRecBinding (v',(ty,substExp rhs bd)))
          (substExp (substExp e (v,Var v')) bd)
    | otherwise 
    = Let (NonRecBinding (v,(ty,substExp rhs bd)))
          (substExp e bd)
  where
    v' = newName (freeExp e ++ freeExp r ++ [v]) v
substExp (LET n t e) bd@(nm,r) 
    | n `elem` tyFreeExp r
    = LET n' (tySubstType t (n,TyVar n'))
             (substExp (tySubstExp e (n,TyVar n')) bd)
    | otherwise
    = LET n t (substExp e bd)
  where
    n' = newName (tyFreeExp e ++ tyFreeExp r ++ freeType t ++ [n]) n
substExp (Case e alts optDef) bd =
  Case (substExp e bd)
       (map (\ alt -> substAlt alt bd) alts)
       (fmap (\ e -> substExp e bd) optDef)
substAlt (Pat lit bds,e) bd@(nm,r)
   = (Pat lit bds',e')
  where (bds',e') = substAlt' bds e bd

substAlt' [] e bd@(nm,r)
  = ([],substExp e bd)
substAlt' ((v,t):rs) e bd@(nm,r)
    | nm == v
    = (((v,t):rs),e)
    | v `elem` freeExp r
    = let (rs',e') = substAlt' rs (substExp e (v,Var v')) bd
      in ((v',t):rs',e')
    | otherwise
    = let (rs',e') = substAlt' rs e bd
      in ((v,t):rs',e')
  where
    v' = newName (freeExp e ++ freeExp r ++ (v : map fst rs)) v




-- returns an ordered list of bindings,
--  and a list of binding renaming to be applied everywhere 
--  except the subtitution itself.

-- Top bindings names can not be renamed using subst,
-- but their right hand side can be.

substTopBinding :: Binding -> (Name,Exp) -> Maybe Binding
substTopBinding (NonRecBinding (v,(ty,rhs))) bd@(nm,r)
    | v == nm
    = Nothing	-- will mess up other bindings
    | v `elem` freeExp r
    = Nothing	-- will be messed up by this binding
    | otherwise
    = Just $ NonRecBinding (v,(ty,substExp rhs bd))

tySubstExp :: Exp -> (Name,Type) -> Exp
tySubstExp (Var n) _ = Var n
tySubstExp (Lit n) _ = Lit n
tySubstExp (App e1 e2) bd = App (tySubstExp e1 bd) (tySubstExp e2 bd) 
tySubstExp (APP e t) bd   = APP (tySubstExp e bd) (tySubstType t bd)
tySubstExp (Lam n t e) bd@(nm,r)
   = Lam n (tySubstType t bd) (tySubstExp e bd)
tySubstExp (LAM n e) bd@(nm,r)
    | n == nm
    = LAM n e
    | n `elem` freeType r
    = LAM n' (tySubstExp (tySubstExp e (n,TyVar n')) bd)
    | otherwise
    = LAM n (tySubstExp e bd)
  where
    n' = newName (tyFreeExp e ++ freeType r ++ [n]) n

tySubstExp (Let (NonRecBinding (v,(ty,rhs))) e) bd@(nm,r) 
    = Let (NonRecBinding (v,(tySubstType ty bd,tySubstExp rhs bd)))
          (tySubstExp e bd)

tySubstExp (LET n t e) bd@(nm,r) 
    | n == nm 
    = LET n t e
    | n `elem` freeType r
    = LET n' (tySubstType t (n,TyVar n'))	-- a = X a, for example.
             (tySubstExp (tySubstExp e (n,TyVar n')) bd)
    | otherwise
    = LET n t (tySubstExp e bd)
  where
    n' = newName (tyFreeExp e ++ freeType r ++ freeType t ++ [n]) n
tySubstExp (Case e alts optDef) bd =
  Case (tySubstExp e bd)
       (map (\ alt -> tySubstAlt alt bd) alts)
       (fmap (\ e -> tySubstExp e bd) optDef)

tySubstAlt (Pat lit bds,e) bd
   = (Pat lit [ (v,tySubstType t bd)
                | (v,t) <- bds
              ]
     , tySubstExp e bd
     )

freeExp :: Exp -> [Name]
freeExp (Var n) = [n]
freeExp (Lit _) = []
freeExp (App e1 e2) = nub (freeExp e1 ++ freeExp e2)
freeExp (APP e t) = freeExp e
freeExp (Lam n t e) = freeExp e \\ [n]
freeExp (LAM n e) = freeExp e 
freeExp (Let (NonRecBinding (n,(_,d))) e) = nub (freeExp e ++ freeExp d) \\ [n]
freeExp (Let (RecBindings binds) e) = nub (freeExp e ++ frees) \\ names
    where 
       names = map fst binds
       frees = concat [ freeExp e | (_,(_,e)) <- binds ]
freeExp (Case e alts def) =
    nub (freeExp e ++
         concat [ freeExp e \\ map fst bds | (Pat _ bds,e) <- alts ] ++
         case def of
           Nothing -> []
           Just d  -> freeExp d)
freeExp e = error $ show e


freeType :: Type -> [Name]
freeType (TyCon _ tys)  = nub (concatMap freeType tys)
freeType (TyVar v)      = [v]
freeType (TyForAll n t) = freeType t \\ [n]

tyFreeExp :: Exp -> [Name]
tyFreeExp (Var n) = []
tyFreeExp (Lit _) = []
tyFreeExp (App e1 e2) = nub (tyFreeExp e1 ++ tyFreeExp e2)
tyFreeExp (APP e t) = tyFreeExp e
tyFreeExp (Lam n t e) = nub (freeType t ++ tyFreeExp e)
tyFreeExp (LAM n e) = tyFreeExp e \\ [n] 
tyFreeExp (Let (NonRecBinding (n,(_,d))) e) = nub (tyFreeExp e ++ tyFreeExp d) 
tyFreeExp (Let (RecBindings binds) e) = nub (tyFreeExp e ++ frees)
    where 
       frees = concat [ tyFreeExp e | (_,(_,e)) <- binds ]
tyFreeExp (LET v t e) = tyFreeExp e \\ [v]
tyFreeExp (Case e alts def) =
    nub (tyFreeExp e ++
         concat [ tyFreeExp e | (Pat _ bds,e) <- alts ] ++
         case def of
           Nothing -> []
           Just d  -> tyFreeExp d)
tyFreeExp e = error $ show e

newName :: [Name] -> Name -> Name
newName frees suggest = 
	head [ nm | nm <- suggest : suggests
             , nm `notElem` frees
             ]
   where suggests = [ suggest ++ "_" ++ show n | n <- [1..]]


tySpecialize :: Type -> Type -> Either String Type
tySpecialize (TyForAll a ty) b = Right $ tySubstType ty (a,b)
tySpecialize ty a              = Left $ "tyApp: " ++ show (ty,a)


{-
tyApp :: Type -> Type -> Type
tyApp (TyCon "->" [a,b]) c = 
ForAll a ty) b = tySubst ty a b
tyApp ty a = error $ "tyApp: " ++ show (ty,a)
-}

tySubstType :: Type -> (Name,Type) -> Type
tySubstType (TyCon lit tys) (a,b) = TyCon lit (map (\ t -> tySubstType t (a,b)) tys)
tySubstType ty@(TyVar n) (a,b) 
    | n == a = b
    | otherwise = ty
tySubstType ty@(TyForAll n ty2) (a,b) 
    | n == a = ty
    | otherwise = TyForAll n (tySubstType ty2 (a,b))
