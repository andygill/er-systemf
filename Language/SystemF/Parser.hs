module Language.SystemF.Parser where

import Language.SystemF.Syntax
import Text.ParserCombinators.Parsec hiding (runParser,char,string,space)
import qualified Text.ParserCombinators.Parsec as P
import Data.Char
import Data.List
import Control.Monad
import Data.Maybe
-- TODO: RM
type P a = Parser a

------------------------------------------------------------------------------

parseName :: Parser String
parseName = lexeme $ primParseName

parseContextName :: Parser String
parseContextName = lexeme $ primParseContextName

parseLit :: Parser String
parseLit = lexeme $ primParseLit

symbol :: Char -> Parser ()
symbol ch = lexeme (do P.char ch ; return ())

-- add a keyword combinator

symbol' :: String -> Parser ()
symbol' str = lexeme (do P.string str ; return ())

parseSemi :: Parser ()
parseSemi = symbol ';' 

parseOpenParen :: Parser ()
parseOpenParen = symbol '(' 

parseCloseParen :: Parser ()
parseCloseParen = symbol ')' 

parseExp :: Parser Exp
parseExp = (do exp <- parseAExp
               aexps <- many (liftM Right parseAExp <|> 
                              (do symbol '<'
                                  t <- liftM Left parseType
                                  symbol '>'
                                  return t))
               return $ foldl mkAPP exp aexps)
       <|> (do symbol '\\'
               args <- many1 parseNameWithType
               symbol' "->"
               exp <- parseExp
               return $ foldr (\ (e,t) -> Lam e t) exp args)
       <|> (do symbol' "/\\"
               nms <- many1 parseName
               symbol' "->"
               exp <- parseExp
               return $ foldr LAM exp nms)
       <|> (do symbol' "let"
               binds <- parseBinding
               symbol' "in"
               exp <- parseExp
               return $ Let binds exp)
       <|> (do symbol' "case"
               e <- parseExp
               symbol' "of"
               symbol' "{"
               alts <- sepBy 
                        (option Nothing (liftM Just parseAlt))
                        (symbol ';')
               extra <- option Nothing $ do 
                          symbol' "_"
                          symbol' "->"
                          e <- parseExp 
                          return $ Just e
               symbol '}'
               return (Case e (catMaybes alts) extra)
           )
                       

parseNameWithType :: Parser (Name,Type)
parseNameWithType = (do
  n <- parseName 
  symbol' "::"
  t <- parseType
  return (n,t))
   <|> (do symbol '(' ; nt <- parseNameWithType ; symbol ')' ; return nt)

-- AExp == Atomic Expression
parseAExp :: Parser Exp
parseAExp = 
       (liftM Var parseName)
   <|> (do symbol '(' ; e <- parseExp ; symbol ')' ; return e)

parseType :: Parser Type
parseType = do
  do t <- parseType'
     option t (do symbol' "->"
                  t2 <- parseType
                  return $ TyCon "->" [t,t2])

parseType' :: Parser Type
parseType' = 
       (liftM TyVar parseName)
   <|> (do lit <- parseLit
           tys <- many parseType'
           return $ TyCon lit tys)
   <|> (do symbol' "forall"
           n <- parseName
           symbol '.'
           ty <- parseType
           return (TyForAll n ty))
   <|> (do symbol '(' ; t <- parseType ; symbol ')' ; return t)

mkAPP e1 (Right e2) = App e1 e2
mkAPP e1 (Left  t2) = APP e1 t2

parseBind :: Parser Bind
parseBind = do nm <- parseName
               symbol' "::"
               ty <- parseType
               symbol' "="
               exp <- parseExp
               return $ (nm,(ty,exp))

parseBinding :: Parser Binding
parseBinding = liftM RecBindings
             $ liftM catMaybes
             $ sepBy 
                        (option Nothing (liftM Just parseBind))
                        (symbol ';')


parseProgram :: Parser Program
parseProgram = do
  binds <- parseBinding
           -- single recusive group
  return $ Program [] [binds]

--parseContext :: Parser (Name,Context)
--parseContext = 

parseAlt :: Parser (Pat,Exp)
parseAlt = do
  consName <- parseLit
  args     <- many $ parseParens $ do
                   n <- parseName
                   symbol' "::"
                   t <- parseType
                   return (n,t)
  symbol' "->"
  e <- parseExp
  return (Pat consName args,e)
                                
------------------------------------------------------------------------------

parseParens :: Parser a -> Parser a
parseParens p = do
  symbol '('
  r <- p
  symbol ')'
  return r

------------------------------------------------------------------------------

whiteSpace :: P ()
whiteSpace = skipMany (P.space <|> comment)
  where
    comment = try  (P.string "--" >> many (noneOf "\n") >> P.char '\n')

lexeme :: P a -> P a 
lexeme p = do { r <- try p; whiteSpace; return r }

primParseName :: P String
primParseName = do c <- satisfy first
                   cs <- many $ satisfy rest
                   if (c:cs) `elem` keywords 
                      then fail "keywords" 
                      else return ()
                   return $ c : cs
  where
    first c = isLower c || c == '_'
    rest  c = first c || isDigit c || isUpper c

primParseContextName :: P String
primParseContextName = do c <- P.char '$'
                          cs <- many1 $ satisfy rest
                          if (c:cs) `elem` keywords 
                            then fail "keywords" 
                            else return ()
                          return $ c : cs
  where 
   rest  c = isLower c || isDigit c || isUpper c

primParseLit :: P String
primParseLit = (do c <- satisfy first
                   cs <- many $ satisfy rest
                   return $ c : cs)
           <|> (do many1 $ digit)
  where
    first c = isUpper c 
    rest  c = first c || isDigit c || isLower c || c == '_'

symChars = "!#$%^&*-_+=|<>.?/'"
keywords = ["case","of","let","in","forall"]	-- ids that are now allowed as ids
symbols  = ["="]			        -- symbolic names that are also not allowed


------------------------------------------------------------------------------

testParser :: Parser a -> String -> Either ParseError a
testParser p input = parse (do { whiteSpace ; r<-  p ;  eof ; return r}) "" input

runParser :: Parser a -> String -> IO a
runParser p input
        = case testParser p input of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          ; error "done"
                          }
            Right x  -> return x

