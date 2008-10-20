module Language.SystemF.Pretty where

import Language.SystemF.Syntax
import Text.PrettyPrint.MarkedHughesPJ hiding (Doc)
import Data.Maybe

type Doc = MDoc (Scope Markup)

data Scope a = Open a | Close a

data Markup = Keyword | Symbol | Edge Int

pprintExp :: Exp -> Doc
pprintExp (Var name) = text name
pprintExp (Lit lit)   = pprintLit lit
pprintExp (App e1 e2) = pprintApp e1 [Right e2]
pprintExp (APP e1 t)  = pprintApp e1 [Left t]
pprintExp (Lam nm t exp)  = sep [ text "\\" 
                                  <+> text nm 
                                  <+> text "::" 
                                  <+> pprintType t 
                                  <+> text "->"
                                , pprintExp exp 
                                ]
pprintExp (LAM nm exp)  = sep [ text "/\\" <+> text nm <+> text "->" 
                                      , pprintExp exp
                                      ]
pprintExp (Let binds exp) = (vcat [ text "let" <+> pprintBinding binds
                                        , text "in" <+> pprintExp exp
                                        ])
pprintExp (Case e alts def) = vcat [ text "case" <+> pprintExp e <+> text "of" 
                                   , nest 2 $ vcat ( all_alts ++ [ text "}" ])
                                   ]
  where
    all_alts = [ text [start] <+> text cons <+> (sep (map ppBind binders)) <+> text "->" <+> pprintExp e
               | (start,(Pat cons binders,e)) <- zip ('{' : repeat ';') alts
               ] ++
               [ text ";" <+> text "_" <+> text "->" <+> pprintExp d
               | d <- maybeToList def
               ]

    ppBind (n,e) = parens $ text n <> text "::" <> pprintType e
pprintExp e = error (show e)
                              

pprintApp (App e1 e2) args = pprintApp e1 (Right e2 : args)
pprintApp (APP e1 t) args  = pprintApp e1 (Left t : args)
pprintApp atom       args  = pprintAtom atom <+> (sep (map pprintArgOrType args))
pprintArgOrType (Left t)  = text "<" <> pprintAtomicType t <> text ">"
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
pprintType (TyCon "->" [t1,t2]) = pprintAtomicType t1 <+> text "->" <+> pprintType t2
pprintType (TyCon lit tys) = pprintLit lit <+> (sep (map pprintAtomicType tys))
pprintType (TyForAll nm ty)  = text "forall" <+> text nm <+> text "." <+> pprintType ty
pprintType (TyVar nm)      = text nm

pprintLit lit = text lit

pprintBind :: Bind -> Doc
pprintBind (name,(ty,rhs)) = nest 2 $ sep [ text name <+> text "::" <+> pprintType ty <+> text "="
                                          , pprintExp rhs <> text ";"
                                          ]

pprintBinding :: Binding -> Doc
pprintBinding (NonRecBinding bind) = pprintBind bind
pprintBinding (RecBindings binds)  = (vcat $ [ text "-- rec group"
                                              ] ++ map pprintBind binds ++
                                             [ text "-- end rec group"
                                             ])


pprintProgram (Program cs bds) = 
    vcat [ {-  pprintContexts cs 
         , -} vcat (map pprintBinding bds)
         ]

