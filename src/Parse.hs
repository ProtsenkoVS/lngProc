{-# OPTIONS_GHC -Wall #-}
module Parse where

import qualified Text.ParserCombinators.Parsec.Expr as E
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec
import Syntax

{- лексика  
  symbol = ';' | '{' | '}' | '(' | ')' | ','
  identif =  char {digit | char}
  keyword = "int" | "read" | "write" | "if" | "while" | "proc"
  ident   =  identif .... not "int" "read" ""write" "if" "while" "proc" 
  number  = ['-']digit { digit }.
  mulOp   = "*" | "/" | "%".
  addOp   = "+" | "-".
-}
identif :: Parser String
identif = do c  <- letter 
             cs <- many alphaNum  
             return (c:cs)

ident :: Parser String
ident = try( do {nm <- identif;
                if (any(nm==) ["int","read","write","if","while", "proc"])
                    then unexpected ("reserved word " ++ show nm)
                    else return nm 
               } ) 

oper  :: String -> Op -> Parser Op
oper str bop = do {_ <- string str; return bop}

mulOp :: Parser Op   
mulOp = (oper "*" Times) <|> (oper "/" Div) <|> (oper "%" Mod)

addOp :: Parser Op   
addOp = (oper "+" Plus) <|> (oper "-" Minus) 

-- розпізнати всі "порожні" символи в кінці		
lexem :: Parser a -> Parser a
lexem p = do {a <- p; spaces; return a}

identifier :: Parser String
identifier = lexem ident

expOp :: Parser Op -> Parser (Expr -> Expr -> Expr)
expOp p = do {x <- lexem p; return (BinOp x)}

symbol :: String ->  Parser ()
symbol st = lexem (string st >> return ())

reserved :: String -> Parser ()
reserved st = try( lexem( string st >> notFollowedBy alphaNum)) 

number :: Parser Integer
number  = do s   <- option "" (string "-")
             cs  <- many1 digit 
             return (read (s++cs))

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")" 

braces :: Parser a -> Parser a
braces p = symbol "{" *> p <* symbol "}"  

commaSep1, semiSep1 :: Parser a -> Parser [a]
commaSep1 p = sepBy1 p (symbol ",")
semiSep1 p  = sepBy1 p (symbol ";")

{- вирази 
  factor = '(' expr ')' | integer | identifier
  term   = factor { mulOp factor }
  expr  = term { addOp term }
-}


factor :: Parser Expr
factor = parens expr
     <|> (lexem number >>= return . Const)
     <|> (identifier >>=  return . Var)
     <?> "factor"

term :: Parser Expr     
term = factor `chainl1` (expOp mulOp)   

expr :: Parser Expr
expr = term `chainl1` (expOp addOp)

{- оператори
  stmt   = "while" '(' expr ')' stmt  
         | "if" '(' expr ')' stmt 
         | "read" iden | "write" expr  
         | iden ( assSt | callSt) 
         | '{' [defin] {defPr} stmt {';' stmt} '}'  
  assSt  = ":=" expr 
  callSt = '(' [iden {',' iden}] ')'
  defin  = "int" iden {',' iden} ';'
  defPr  = "proc" iden proced ';'
  proced = '(' [iden {',' iden}] ')' stmt
                                               listId = iden {',' iden}
                                                listSt = stmt {';' stmt}  
  program= stmt eos 
-}   

stmt :: Parser Stmt 
stmt = (reserved "while" >> While <$> parens expr <*> stmt)
   <|> (reserved "if" >> If <$> parens expr <*> stmt)
   <|> (reserved "read" >>  Read <$> identifier)
   <|> (reserved "write" >> Write <$> expr)
   <|> (identifier >>= (\v -> assignSt v <|> callSt v))
   <|> (braces (Block <$> (option [] defin) <*> (many defPr) <*> semiSep1 stmt))
   <?> "statement"

assignSt :: String -> Parser Stmt 
assignSt var = symbol ":=" >> Assign var <$> expr

callSt :: String -> Parser Stmt 
callSt var = parens (Call var <$> (option [] (commaSep1 identifier)))

defin :: Parser [String] 
defin = reserved "int" *> commaSep1 identifier <* symbol ";"

defPr :: Parser (String,Proc)
defPr = reserved "proc" >> (,) <$> identifier <*> proced

proced :: Parser Proc
proced = (,) <$>  parens (option [] (commaSep1 identifier)) <*> stmt

program :: Parser Program 
program = spaces *> stmt <* eof

--------------------------------------------------

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser (
           emptyDef {identStart = letter
                    ,identLetter = alphaNum 
                    ,reservedNames = ["int","read","write","if","while","proc"]
                    ,reservedOpNames = ["*","/","%","+","-"]}
                           )

factorS :: Parser Expr
factorS  =  P.parens lexer expr 
       <|> (P.integer lexer >>= return . Const)
       <|> (P.identifier lexer >>= return . Var) 

table :: E.OperatorTable Char () Expr
table  = [[op "*" Times E.AssocLeft, op "/" Div E.AssocLeft, op "%" Mod E.AssocLeft]
         ,[op "+" Plus E.AssocLeft, op "-" Minus E.AssocLeft]
         ]
         where
            op s f assoc = E.Infix (P.reservedOp lexer s >> return (Syntax.BinOp f)) assoc

exprS :: Parser Expr
exprS  = E.buildExpressionParser table factorS   -- <?> "expression"

stmtS :: Parser Stmt 
stmtS = (P.reserved lexer "while" >> While <$> P.parens lexer exprS <*> stmtS)
   <|> (P.reserved lexer "if" >> If <$> P.parens lexer exprS <*> stmtS)
   <|> (P.reserved lexer "read" >> Read <$> P.identifier lexer)
   <|> (P.reserved lexer "write">> Write <$> exprS)
   <|> (P.identifier lexer >>= (\v -> assignStS v <|> callStS v))
   <|> (P.braces lexer (Block <$> (option [] definS) <*> (many defPrS) <*> P.semiSep lexer stmt))

assignStS :: String -> Parser Stmt 
assignStS var = symbol ":=" >> Assign var <$> exprS

callStS :: String -> Parser Stmt 
callStS var = P.parens lexer  (Call var <$> (option [] (P.commaSep1 lexer (P.identifier lexer))))

definS :: Parser [String] 
definS = P.reserved lexer "int" *> P.commaSep1 lexer (P.identifier lexer) <* P.symbol lexer ";"

defPrS :: Parser (String,Proc)
defPrS = P.reserved lexer "proc" >> (,) <$> (P.identifier lexer) <*> procedS

procedS :: Parser Proc
procedS = (,) <$>  P.parens lexer (option [] (P.commaSep1 lexer (P.identifier lexer))) <*> stmtS

programL :: Parser Program 
programL = P.whiteSpace lexer *> stmtS <* eof

--------------------------------------------------

parsePL :: ParseOpt -> String -> Program  -- ParserO | LibraryO
parsePL p s = let r = if p==ParserO then parse program "" s
                                    else parse programL "" s  
              in case r of
                 Left _   -> error "Syntax" 
                 Right pr -> pr  

parsePLL :: String -> Program
parsePLL s = case parse programL "" s of
               Left _  -> error "Syntax"
               Right e -> e

