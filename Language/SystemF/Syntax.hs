module Language.SystemF.Syntax where

type Name = String	-- lower case
type Lit  = String	-- upper case or numerals

data Exp = Var Name
         | Lit Lit
         | App Exp Exp
         | APP Exp Type
         | Lam Name Type Exp
         | LAM Name Exp
         | Let Binding Exp
         | LET Name Type Exp
         | Case Exp [(Pat,Exp)] (Maybe Exp)
           deriving Show

data Binding = NonRecBinding Bind
             | RecBindings [Bind]	-- (v1,..,vn) = fix (\ (v1,..,vn) -> ...)
           deriving Show

type Bind = (Name,(Type,Exp))

data Type = TyCon Lit [Type]
          | TyVar Name
          | TyForAll Name Type
           deriving Show

type TypeEnv = [(Name,Type)]

data Pat = Pat Lit [(Name,Type)]
           deriving Show

data Program = Program TypeEnv [Binding]

