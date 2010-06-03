module Language.SystemF.Pretty where

import Language.SystemF.Syntax
import Text.PrettyPrint.MarkedHughesPJ hiding (Doc)
import Data.Maybe

type Doc = MDoc (Scope Markup)

data Scope a = Open a | Close a

data Markup = Keyword | Symbol | Comment | Edge Int


------------------------------------------------------------------------------

scope :: Markup -> Doc -> Doc
scope m d = mark (Open m) <> d <> mark (Close m)

keyword :: String -> Doc
keyword = scope Keyword . text 

symbol :: String -> Doc
symbol = scope Symbol . text 

comment :: String -> Doc
comment = scope Comment . text 

------------------------------------------------------------------------------


pprintExp :: Exp -> Doc
pprintExp (Var name) = text name
pprintExp (Lit lit)   = pprintLit lit
pprintExp (App e1 e2) = pprintApp e1 [Right e2]
pprintExp (APP e1 t)  = pprintApp e1 [Left t]
pprintExp (Lam nm t exp)  = sep [ symbol "\\" 
                                  <+> text nm 
                                  <+> symbol "::" 
                                  <+> pprintType t 
                                  <+> symbol "->"
                                , pprintExp exp 
                                ]
pprintExp (LAM nm exp)  = sep [ symbol "/\\" <+> text nm <+> symbol "->" 
                                      , pprintExp exp
                                      ]
pprintExp (Let binds exp) = (vcat [ keyword "let" <+> pprintBinding binds
                                        , keyword "in" <+> pprintExp exp
                                        ])
pprintExp (Case e alts def) = vcat [ keyword "case" <+> pprintExp e <+> keyword "of" 
                                   , nest 2 $ vcat ( all_alts ++ [ symbol "}" ])
                                   ]
  where
    all_alts = [ symbol [start] <+> text cons <+> (sep (map ppBind binders)) <+> symbol "->" <+> pprintExp e
               | (start,(Pat cons binders,e)) <- zip ('{' : repeat ';') alts
               ] ++
               [ symbol ";" <+> text "_" <+> symbol "->" <+> pprintExp d
               | d <- maybeToList def
               ]

    ppBind (n,e) = parens $ text n <> symbol "::" <> pprintType e
pprintExp e = error (show e)
                              

pprintApp (App e1 e2) args = pprintApp e1 (Right e2 : args)
pprintApp (APP e1 t) args  = pprintApp e1 (Left t : args)
pprintApp atom       args  = pprintAtom atom <+> (sep (map pprintArgOrType args))
pprintArgOrType (Left t)  = symbol "<" <> pprintAtomicType t <> symbol ">"
pprintArgOrType (Right e) = pprintAtom e

pprintAtom e | isAtom e  = pprintExp e
             | otherwise = parens (pprintExp e)

isAtom (Var {}) = True
isAtom (Lit {}) = True
isAtom _        = False

pprintAtomicType t@(TyCon lit tys) | length tys == 0 = pprintType t
pprintAtomicType t@(TyVar nm) = pprintType t
pprintAtomicType t = parens (pprintType t)

pprintType (TyCon lit [])  = pprintLit lit
pprintType (TyCon "->" [t1,t2]) = pprintAtomicType t1 <+> symbol "->" <+> pprintType t2
pprintType (TyCon lit tys) = pprintLit lit <+> (sep (map pprintAtomicType tys))
pprintType (TyForAll nm ty)  = keyword "forall" <+> text nm <+> symbol "." <+> pprintType ty
pprintType (TyVar nm)      = text nm

pprintLit lit = text lit

pprintBind :: Bind -> Doc
pprintBind (name,(ty,rhs)) = nest 2 $ sep [ text name <+> symbol "::" <+> pprintType ty <+> symbol "="
                                          , pprintExp rhs <> symbol ";"
                                          ]

pprintBinding :: Binding -> Doc
pprintBinding (NonRecBinding bind) = pprintBind bind
pprintBinding (RecBindings binds)  = (vcat $ [ comment "-- rec group"
                                              ] ++ map pprintBind binds ++
                                             [ comment  "-- end rec group"
                                             ])


pprintProgram (Program cs bds) = 
    vcat [ {-  pprintContexts cs 
         , -} vcat (map pprintBinding bds)
         ]

