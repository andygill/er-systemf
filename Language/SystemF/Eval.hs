module Language.SystemF.Eval where

import Language.SystemF.Syntax

data Value = ValCons Lit [Type] [Value]
           | ValApp Name (Value -> Value)
           | ValTyApp Name (Type -> Value)

instance Show Value where
    show (ValCons lit [] []) = lit
    show (ValCons lit tys vals) = lit ++ " <" ++ show tys ++ "> " ++ show vals
    show (ValApp n _)   = "(\\ " ++ show n ++ " ...)"
    show (ValTyApp n _) = "(/\\ " ++ show n ++ " ...)"

-- We are using the lazy semantics of Haskell to give the lazy semantics of this eval/apply
eval :: [(Name,Value)] -> Exp -> Value
eval env (Var n) = case lookup n env of
                     Nothing -> error $ "can not find var name: " ++ show n ++ "\n" ++ show (map fst env) ++ show (take 20 env)
                     Just v -> v
eval env (Lit lit) = ValCons lit [] []
eval env (App e1 e2)   = apply env (eval env e1) (eval env e2)
eval env (APP e1 t)    = applyTy env (eval env e1) t
eval env (Lam n t e)   = ValApp n  $ \ v -> eval ((n,v):env) e	-- TODO: include the type in this
eval env (LAM n e)     = ValTyApp n $ \ t -> eval env e
eval env (Let (NonRecBinding (n,(_,d))) e) = eval env' e
  where
    env'   = (n,eval env d) : env
eval env (Let (RecBindings binds) e) = eval env' e
  where
    env'   = binds' ++ env
    binds' = [ (n,eval env' e) | (n,(ty,e)) <- binds ]
eval env (Case e alts def) = evalCase env (eval env e) alts def

apply :: [(Name,Value)] -> Value -> Value -> Value
apply env (ValApp _ f) arg         = f arg
apply env v@(ValTyApp n f) val     = error $ "attemping to apply a type-lambda expression to value\n" 
                                        ++ "(" ++ show v ++ ")" ++ "(" ++ show val ++ ")"
apply env (ValCons lit tys args) arg = ValCons lit tys (args ++ [arg])

applyTy :: [(Name,Value)] -> Value -> Type -> Value
applyTy env v@(ValApp n f) ty        = error $ "attemping to apply a lambda expression to a type\n" 
                                        ++ "(" ++ show v ++ ")" ++ "(" ++ show ty ++ ")"
applyTy env (ValTyApp n f) ty      = f ty
applyTy env (ValCons lit tys []) t = ValCons lit (tys ++ [t]) []
applyTy env v@(ValCons lit tys args) t   = error $ "type application of " ++ show v ++ " to " ++ show t

{-
subst :: Name -> Value -> Exp -> Exp
subst n v (Var m)     | n == m = v
subst n v (App e1 e2) = App (subst n v e1) (subst n v e2)
subst n v (Lam m t e)  | n /= m = Lam m t (subst n v e)
subst n v e = e -- otherwise
-}

evalCase :: [(Name,Value)] -> Value -> [(Pat,Exp)] -> Maybe Exp -> Value
evalCase env (ValCons lit _ args) [] (Just def) = eval env def
evalCase env (ValCons lit _ args) [] Nothing = error "failing case with no default"
evalCase env (ValCons lit tys args) ((Pat lit' args',e):alts) def 
    | lit == lit' && length args /= length args' = error "argument count mis-match in case (opps!)"
    | lit == lit' = eval ([ (n,v)
                         | (v,(n,t)) <- zip args args'
                         ] ++ env) e
    | otherwise = evalCase env (ValCons lit tys args) alts def
evalCase env _ _ _ = error "attempting to case a lambda expression"

{-
testE example = do
  str <- readFile "Rev.ww"
  print str
  p@(Program cs bds) <- runParser parseProgram str
  let (Right e) = testParser parseExp example
  print e
  let doc = pprintExp e
  hPutDoc stdout doc
  putStrLn ""
  let env = []
  print (eval env (foldr Let e bds))
  print (freeExp e)
{-
  let p'@(Program cs' bds') = addFix p
  let doc = pprintProgram p'
  hPutDoc stdout doc
  putStrLn ""
  print (eval env (foldr Let e bds'))
  print (freeExp e)
-}

-- t = testE "reverse @ (I) (Cons 1 (Cons 2 (Cons 3 Nil)))"

-}

